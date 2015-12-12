-- Copyright Corey O'Connor
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

module Graphics.Vty.Output.Interface
where

import Graphics.Vty.Prelude

import Graphics.Vty.Picture
import Graphics.Vty.PictureToSpans
import Graphics.Vty.Span

import Graphics.Vty.DisplayAttributes

import Control.Monad.Operational
import Control.Monad.Trans

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (mempty, mappend)
#endif

data Output = Output
    { -- | Text identifier for the output device. Used for debugging.
      terminalID :: String
    , releaseTerminal :: forall m. MonadIO m => m ()
    -- | Clear the display and initialize the terminal to some initial display state.
    --
    -- The expectation of a program is that the display starts in some initial state.
    -- The initial state would consist of fixed values:
    --
    --  - cursor at top left
    --  - UTF-8 character encoding
    --  - drawing characteristics are the default
    --
    -- The abstract operation I think all these behaviors are instances of is reserving exclusive
    -- access to a display such that:
    --
    --  - The previous state cannot be determined
    --  - When exclusive access to a display is released the display returns to the previous state.
    , reserveDisplay :: forall m. MonadIO m => m ()
    -- | Return the display to the state before `reserveDisplay`
    -- If no previous state then set the display state to the initial state.
    , releaseDisplay :: forall m. MonadIO m => m ()
    -- | Returns the current display bounds.
    , displayBounds :: forall m. MonadIO m => m DisplayRegion
    -- | Serialize the display commands to the terminal device.
    , outputDisplayCommands :: forall a m. MonadIO m => DisplayCommands a -> m a
    -- | Maximum number of colors supported by the context.
    , contextColorCount :: Int
    -- | if the cursor can be shown / hidden
    , supportsCursorVisibility :: Bool
    , assumedStateRef :: IORef AssumedState
    }

data AssumedState = AssumedState
    { prevFattr :: Maybe FixedAttr
    , prevOutputOps :: Maybe DisplayOps
    }

initialAssumedState :: AssumedState
initialAssumedState = AssumedState Nothing Nothing

data DisplayContext = DisplayContext
    { contextDevice :: Output
    -- | Provide the bounds of the display context.
    , contextRegion :: DisplayRegion
    }

data DisplayCommand cmd where
    -- Sets the output position to the specified row and column. Where the number of bytes
    -- required for the control codes can be specified seperate from the actual byte sequence.
    MoveCursor :: Int -> Int -> DisplayCommand ()
    ShowCursor :: DisplayCommand ()
    HideCursor :: DisplayCommand ()
    -- Assure the specified output attributes will be applied to all the following text until the
    -- next output attribute change. Where the number of bytes required for the control codes can
    -- be specified seperate from the actual byte sequence.  The required number of bytes must be
    -- at least the maximum number of bytes required by any attribute changes.  The serialization
    -- equations must provide the ptr to the next byte to be specified in the output buffer.
    --
    -- The currently applied display attributes are provided as well. The Attr data type can
    -- specify the style or color should not be changed from the currently applied display
    -- attributes. In order to support this the currently applied display attributes are required.
    -- In addition it may be possible to optimize the state changes based off the currently applied
    -- display attributes.
    SetAttr :: FixedAttr -> Attr -> DisplayAttrDiff -> DisplayCommand ()
    -- Reset the display attributes to the default display attributes
    DefaultAttr :: DisplayCommand ()
    -- End of displayed content on this row.
    DisplayRowEnd :: DisplayCommand ()
    -- All terminfo terminals serialize UTF8 text to the terminal device exactly as serialized in memory.
    Utf8Text  :: BS.ByteString -> DisplayCommand ()

type DisplayCommands a = Program DisplayCommand a

-- | Renders the given `Picture` to the given `DisplayContext`.
--
-- This transforms the picture to `DisplayCommands` then outputs those commands.
--
--      0. The image is cropped to the display size.
--
--      1. Converted into a sequence of attribute changes and text spans.
--
--      2. The cursor is hidden.
--
--      3. Serialized to the display.
--
--      4. The cursor is then shown and positioned or kept hidden.
--
-- todo: specify possible IO exceptions.
-- abstract from IO monad to a MonadIO instance.
outputPictureToContext :: MonadIO m => DisplayContext -> Picture -> m ()
outputPictureToContext dc pic = liftIO $ do
    as <- readIORef (assumedStateRef $ contextDevice dc)
    let out = renderPicture dc as pic
    ops <- outputDisplayCommands (contextDevice dc) out
    -- Cache the output spans.
    let as' = as { prevOutputOps = Just ops }
    writeIORef (assumedStateRef $ contextDevice dc) as'

-- | Renders the given `Picture` to the default `DisplayContext` of the `Output`.
outputPicture :: MonadIO m => Output -> Picture -> m ()
outputPicture out pic = do
    dc <- DisplayContext out <$> displayBounds out
    outputPictureToContext dc pic

renderPicture :: DisplayContext -> AssumedState -> Picture -> DisplayCommands DisplayOps
renderPicture dc as pic = do
    let r = contextRegion dc
        initialAttr = FixedAttr defaultStyleMask Nothing Nothing
        manipCursor = supportsCursorVisibility (contextDevice dc)
        ops = displayOpsForPic pic r
        -- Diff the previous output against the requested output. Differences are currently on a per-row
        -- basis.
        -- \todo handle resizes that crop the dominate directions better.
        diffs :: [Bool] = case prevOutputOps as of
            Nothing -> replicate (fromEnum $! regionHeight $! effectedRegion ops) True
            Just previousOps -> if effectedRegion previousOps /= effectedRegion ops
                                then replicate (displayOpsRows ops) True
                                else zipWith (/=) (Vector.toList previousOps)
                                     (Vector.toList ops)

    when manipCursor $ singleton HideCursor
    writeDisplayOps ops dc initialAttr diffs
    case picCursor pic of
        _ | not manipCursor -> return ()
        NoCursor            -> return ()
        Cursor x y          -> do
            let m = cursorOutputMap ops $ picCursor pic
                (ox, oy) = charToOutputPos m (x,y)
            singleton ShowCursor
            singleton (MoveCursor ox oy)
    return ops

writeDisplayOps :: DisplayOps -> DisplayContext -> FixedAttr -> [Bool] -> DisplayCommands ()
writeDisplayOps ops dc initialAttr diffs = do
    void $! Vector.ifoldM' writeOutputOps' diffs ops
    where
        writeOutputOps' (True : diffs') y rowOps = do
            writeRowOps rowOps dc y initialAttr
            return diffs'
        writeOutputOps' (False : diffs') _y _rowOps
            = return diffs'
        writeOutputOps' [] _y _rowOps
            = error "vty - output spans without a corresponding diff."

writeRowOps :: RowOps -> DisplayContext -> Int -> FixedAttr -> DisplayCommands ()
writeRowOps rowOps dc y initialAttr = do
    singleton (MoveCursor 0 y)
    singleton DefaultAttr
    void $! Vector.foldM' (\fattr op -> writeSpanOp op dc fattr) initialAttr rowOps

writeSpanOp :: SpanOp -> DisplayContext -> FixedAttr -> DisplayCommands FixedAttr
writeSpanOp (TextSpan attr _ _ str) dc fattr = do
    let attr' = limitAttrForDisplay (contextDevice dc) attr
        fattr' = fixDisplayAttr fattr attr'
        diffs = displayAttrDiffs fattr fattr'
    singleton (SetAttr fattr attr' diffs)
    singleton (Utf8Text (T.encodeUtf8 $! TL.toStrict str))
    return fattr'

-- These should all be filtered after the layers are flattened.
writeSpanOp (Skip _)   _dc _fattr = error "writeSpanOp for Skip"
writeSpanOp (RowEnd _) _dc fattr  = do
    singleton DefaultAttr
    singleton DisplayRowEnd
    return fattr

-- | The cursor position is given in X,Y character offsets. Due to multi-column characters this
-- needs to be translated to column, row positions.
data CursorOutputMap = CursorOutputMap
    { charToOutputPos :: (Int, Int) -> (Int, Int)
    }

cursorOutputMap :: DisplayOps -> Cursor -> CursorOutputMap
cursorOutputMap rowOps _cursor = CursorOutputMap
    { charToOutputPos = \(cx, cy) -> (cursorColumnOffset rowOps cx cy, cy)
    }

cursorColumnOffset :: DisplayOps -> Int -> Int -> Int
cursorColumnOffset ops cx cy =
    let cursorDisplayOps = Vector.unsafeIndex ops (fromEnum cy)
        (outOffset, _, _)
            = Vector.foldl' ( \(d, currentCx, done) op ->
                        if done then (d, currentCx, done) else case spanOpHasWidth op of
                            Nothing -> (d, currentCx, False)
                            Just (cw, ow) -> case compare cx (currentCx + cw) of
                                    GT -> ( d + ow
                                          , currentCx + cw
                                          , False
                                          )
                                    EQ -> ( d + ow
                                          , currentCx + cw
                                          , True
                                          )
                                    LT -> ( d + columnsToCharOffset (cx - currentCx) op
                                          , currentCx + cw
                                          , True
                                          )
                      )
                      (0, 0, False)
                      cursorDisplayOps
    in outOffset

-- | Not all terminals support all display attributes. This filters a display attribute to what the
-- given terminal can display.
limitAttrForDisplay :: Output -> Attr -> Attr
limitAttrForDisplay t attr
    = attr { attrForeColor = clampColor $ attrForeColor attr
           , attrBackColor = clampColor $ attrBackColor attr
           }
    where
        clampColor Default     = Default
        clampColor KeepCurrent = KeepCurrent
        clampColor (SetTo c)   = clampColor' c
        clampColor' (ISOColor v)
            | contextColorCount t < 8            = Default
            | contextColorCount t < 16 && v >= 8 = SetTo $ ISOColor (v - 8)
            | otherwise                          = SetTo $ ISOColor v
        clampColor' (Color240 v)
            -- TODO: Choose closest ISO color?
            | contextColorCount t <  8           = Default
            | contextColorCount t <  16          = Default
            | contextColorCount t <= 256         = SetTo $ Color240 v
            | otherwise
                = let p :: Double = fromIntegral v / 240.0
                      v' = floor $ p * (fromIntegral $ contextColorCount t)
                  in SetTo $ Color240 v'
