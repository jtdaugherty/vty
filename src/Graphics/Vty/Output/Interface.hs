-- Copyright Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module provides an abstract interface for performing terminal
-- output. The only user-facing part of this API is 'Output'.
module Graphics.Vty.Output.Interface
  ( Encoding(..)
  , Output(..)
  , AssumedState(..)
  , DisplayContext(..)
  , Mode(..)
  , displayContext
  , outputPicture
  , initialAssumedState
  , limitAttrForDisplay
  )
where

import Graphics.Vty.Attributes
import Graphics.Vty.Image (DisplayRegion, regionHeight)
import Graphics.Vty.Picture
import Graphics.Vty.PictureToSpans
import Graphics.Vty.Span

import Graphics.Vty.DisplayAttributes

import Blaze.ByteString.Builder (Write, writeToByteString)
import Blaze.ByteString.Builder.ByteString (writeByteString)

import Control.Monad.Trans

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as Vector

-- | Modal terminal features that can be enabled and disabled.
data Mode = Mouse
          -- ^ Mouse mode (whether the terminal is configured to provide
          -- mouse input events)
          | BracketedPaste
          -- ^ Paste mode (whether the terminal is configured to provide
          -- events on OS pastes)
          | Focus
          -- ^ Focus-in/focus-out events (whether the terminal is
          -- configured to provide events on focus change)
          | Hyperlink
          -- ^ Hyperlink mode via the 'withURL' attribute modifier (see
          -- https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda).
          -- Note that this may not work gracefully in all terminal
          -- emulators so be sure to test this mode with the terminals
          -- you intend to support. It is off by default.
          deriving (Eq, Read, Show)

data Encoding = UTF8
              | UTF16Le
              | UTF16Be
              | UTF32Le
              | UTF32Be

-- | The Vty terminal output interface.
data Output = Output
    { -- | Text identifier for the output device. Used for debugging.
      terminalID :: String
      -- | Release the terminal just prior to application exit and reset
      -- it to its state prior to application startup.
    , releaseTerminal :: forall m. MonadIO m => m ()
      -- | Clear the display and initialize the terminal to some initial
      -- display state.
      --
      -- The expectation of a program is that the display starts in some
      -- The initial state. initial state would consist of fixed values:
      --
      --  - cursor at top left
      --  - UTF-8 character encoding
      --  - drawing characteristics are the default
    , reserveDisplay :: forall m. MonadIO m => m ()
      -- | Return the display to the state before `reserveDisplay` If no
      -- previous state then set the display state to the initial state.
    , releaseDisplay :: forall m. MonadIO m => m ()
      -- | Returns the current display bounds.
    , displayBounds :: forall m. MonadIO m => m DisplayRegion
      -- | Specifies the character encoding the terminal requires. 
    , outputEncoding :: Encoding
      -- | Output the bytestring to the terminal device.
    , outputByteBuffer :: BS.ByteString -> IO ()
      -- | Specifies the maximum number of colors supported by the
      -- context.
    , contextColorCount :: Int
      -- | Specifies whether the cursor can be shown / hidden.
    , supportsCursorVisibility :: Bool
      -- | Indicates support for terminal modes for this output device.
    , supportsMode :: Mode -> Bool
      -- | Enables or disables a mode (does nothing if the mode is
      -- unsupported).
    , setMode :: forall m. MonadIO m => Mode -> Bool -> m ()
      -- | Returns whether a mode is enabled.
    , getModeStatus :: forall m. MonadIO m => Mode -> m Bool
    , assumedStateRef :: IORef AssumedState
      -- | Acquire display access to the given region of the display.
      -- Currently all regions have the upper left corner of (0,0) and
      -- the lower right corner at (max displayWidth providedWidth, max
      -- displayHeight providedHeight)
    , mkDisplayContext :: forall m. MonadIO m => Output -> DisplayRegion -> m DisplayContext
      -- | Ring the terminal bell if supported.
    , ringTerminalBell :: forall m. MonadIO m => m ()
      -- | Returns whether the terminal has an audio bell feature.
    , supportsBell :: forall m. MonadIO m => m Bool
    }

displayContext :: MonadIO m => Output -> DisplayRegion -> m DisplayContext
displayContext t = liftIO . mkDisplayContext t t

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
    -- | Sets the output position to the specified row and column
    -- where the number of bytes required for the control codes can be
    -- specified seperate from the actual byte sequence.
    , writeMoveCursor :: Int -> Int -> Write
    , writeShowCursor :: Write
    , writeHideCursor :: Write
    -- Ensure that the specified output attributes will be applied to
    -- all the following text until the next output attribute change
    -- where the number of bytes required for the control codes can be
    -- specified seperately from the actual byte sequence. The required
    -- number of bytes must be at least the maximum number of bytes
    -- required by any attribute changes. The serialization equations
    -- must provide the ptr to the next byte to be specified in the
    -- output buffer.
    --
    -- The currently applied display attributes are provided as well.
    -- The Attr data type can specify the style or color should not be
    -- changed from the currently applied display attributes. In order
    -- to support this the currently applied display attributes are
    -- required. In addition it may be possible to optimize the state
    -- changes based off the currently applied display attributes.
    , writeSetAttr :: Bool -> FixedAttr -> Attr -> DisplayAttrDiff -> Write
    -- | Reset the display attributes to the default display attributes.
    , writeDefaultAttr :: Bool -> Write
    , writeRowEnd :: Write
    -- | See `Graphics.Vty.Output.XTermColor.inlineHack`
    , inlineHack :: IO ()
    }

-- | All terminals serialize UTF8 text to the terminal device exactly as
-- serialized in memory.
writeUtf8Text  :: BS.ByteString -> Write
writeUtf8Text = writeByteString

-- | Encode text to specified character encoding. 
encodeText :: Encoding -> T.Text -> BS.ByteString
encodeText UTF8 = T.encodeUtf8
encodeText UTF16Le = T.encodeUtf16LE
encodeText UTF16Be = T.encodeUtf16BE
encodeText UTF32Le = T.encodeUtf32LE
encodeText UTF32Be = T.encodeUtf32BE

writeText :: Encoding -> T.Text -> Write
writeText encoding = writeByteString . encodeText encoding 

writeLazyText :: Encoding -> TL.Text -> Write
writeLazyText encoding = writeText encoding . TL.toStrict

-- | Displays the given `Picture`.
--
--      1. The image is cropped to the display size.
--
--      2. Converted into a sequence of attribute changes and text spans.
--
--      3. The cursor is hidden.
--
--      4. Serialized to the display.
--
--      5. The cursor is then shown and positioned or kept hidden.
outputPicture :: MonadIO m => DisplayContext -> Picture -> m ()
outputPicture dc pic = liftIO $ do
    urlsEnabled <- getModeStatus (contextDevice dc) Hyperlink
    as <- readIORef (assumedStateRef $ contextDevice dc)
    let manipCursor = supportsCursorVisibility (contextDevice dc)
        r = contextRegion dc
        ops = displayOpsForPic pic r
        initialAttr = FixedAttr defaultStyleMask Nothing Nothing Nothing
        encoding = outputEncoding (contextDevice dc)
        -- Diff the previous output against the requested output.
        -- Differences are currently on a per-row basis.
        diffs :: [Bool] = case prevOutputOps as of
            Nothing -> replicate (fromEnum $ regionHeight $ affectedRegion ops) True
            Just previousOps -> if affectedRegion previousOps /= affectedRegion ops
                then replicate (displayOpsRows ops) True
                else Vector.toList $ Vector.zipWith (/=) previousOps ops
        -- build the Write corresponding to the output image
        out = (if manipCursor then writeHideCursor dc else mempty)
              `mappend` writeOutputOps encoding urlsEnabled dc initialAttr diffs ops
              `mappend`
                (let (w,h) = contextRegion dc
                     clampX = max 0 . min (w-1)
                     clampY = max 0 . min (h-1) in
                 case picCursor pic of
                    _ | not manipCursor -> mempty
                    NoCursor            -> mempty
                    AbsoluteCursor x y ->
                        writeShowCursor dc `mappend`
                        writeMoveCursor dc (clampX x) (clampY y)
                    Cursor x y           ->
                        let m = cursorOutputMap ops $ picCursor pic
                            (ox, oy) = charToOutputPos m (clampX x, clampY y)
                        in writeShowCursor dc `mappend`
                           writeMoveCursor dc (clampX ox) (clampY oy)
                )
    -- ... then serialize
    outputByteBuffer (contextDevice dc) (writeToByteString out)
    -- Cache the output spans.
    let as' = as { prevOutputOps = Just ops }
    writeIORef (assumedStateRef $ contextDevice dc) as'

writeOutputOps :: Encoding -> Bool -> DisplayContext -> FixedAttr -> [Bool] -> DisplayOps -> Write
writeOutputOps encoding urlsEnabled dc initialAttr diffs ops =
    let (_, out, _) = Vector.foldl' writeOutputOps'
                                       (0, mempty, diffs)
                                       ops
    in out
    where
        writeOutputOps' (y, out, True : diffs') spanOps
            = let spanOut = writeSpanOps encoding urlsEnabled dc y initialAttr spanOps
                  out' = out `mappend` spanOut
              in (y+1, out', diffs')
        writeOutputOps' (y, out, False : diffs') _spanOps
            = (y + 1, out, diffs')
        writeOutputOps' (_y, _out, []) _spanOps
            = error "vty - output spans without a corresponding diff."

writeSpanOps :: Encoding -> Bool -> DisplayContext -> Int -> FixedAttr -> SpanOps -> Write
writeSpanOps encoding urlsEnabled dc y initialAttr spanOps =
    -- The first operation is to set the cursor to the start of the row
    let start = writeMoveCursor dc 0 y `mappend` writeDefaultAttr dc urlsEnabled
    -- then the span ops are serialized in the order specified
    in fst $ Vector.foldl' (\(out, fattr) op -> case writeSpanOp encoding urlsEnabled dc op fattr of
                              (opOut, fattr') -> (out `mappend` opOut, fattr')
                           )
                           (start, initialAttr)
                           spanOps

writeSpanOp :: Encoding -> Bool -> DisplayContext -> SpanOp -> FixedAttr -> (Write, FixedAttr)
writeSpanOp encoding urlsEnabled dc (TextSpan attr _ _ str) fattr =
    let attr' = limitAttrForDisplay (contextDevice dc) attr
        fattr' = fixDisplayAttr fattr attr'
        diffs = displayAttrDiffs fattr fattr'
        out =  writeSetAttr dc urlsEnabled fattr attr' diffs
               `mappend` writeLazyText encoding str
    in (out, fattr')
writeSpanOp _ _ _ (Skip _) _fattr = error "writeSpanOp for Skip"
writeSpanOp _ urlsEnabled dc (RowEnd _) fattr = (writeDefaultAttr dc urlsEnabled `mappend` writeRowEnd dc, fattr)

-- | The cursor position is given in X,Y character offsets. Due to
-- multi-column characters this needs to be translated to column, row
-- positions.
data CursorOutputMap = CursorOutputMap
    { charToOutputPos :: (Int, Int) -> (Int, Int)
    }

cursorOutputMap :: DisplayOps -> Cursor -> CursorOutputMap
cursorOutputMap spanOps _cursor = CursorOutputMap
    { charToOutputPos = \(cx, cy) -> (cursorColumnOffset spanOps cx cy, cy)
    }

cursorColumnOffset :: DisplayOps -> Int -> Int -> Int
cursorColumnOffset ops cx cy =
    let cursorRowOps = Vector.unsafeIndex ops (fromEnum cy)
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
                      cursorRowOps
    in outOffset

-- | Not all terminals support all display attributes. This filters a
-- display attribute to what the given terminal can display.
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
            -- Should we choose closest ISO color?
            | contextColorCount t <  8           = Default
            | contextColorCount t <  16          = Default
            | contextColorCount t <= 256         = SetTo $ Color240 v
            | otherwise
                = let p :: Double = fromIntegral v / 240.0
                      v' = floor $ p * (fromIntegral $ contextColorCount t)
                  in SetTo $ Color240 v'
