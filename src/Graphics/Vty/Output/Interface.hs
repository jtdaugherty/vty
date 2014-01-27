-- Copyright Corey O'Connor
-- General philosophy is: MonadIO is for equations exposed to clients.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Vty.Output.Interface
where

import Graphics.Vty.Prelude

import Graphics.Vty.Picture
import Graphics.Vty.PictureToSpans
import Graphics.Vty.Span

import Graphics.Vty.DisplayAttributes

import Blaze.ByteString.Builder (Write)
import Blaze.ByteString.Builder.ByteString (writeByteString)

import Control.Monad.Trans

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.IORef
import qualified Data.Vector.Unboxed as Vector 

data Output = Output
    { -- | Text identifier for the output device. Used for debugging. 
      terminal_ID :: String
    , release_terminal :: MonadIO m => m ()
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
    , reserve_display :: MonadIO m => m ()
    -- | Return the display to the state before reserve_display
    -- If no previous state then set the display state to the initial state.
    , release_display :: MonadIO m => m ()
    -- | Returns the current display bounds.
    , display_bounds :: MonadIO m => m DisplayRegion
    -- | Output the byte string to the terminal device.
    , output_byte_buffer :: BS.ByteString -> IO ()
    -- | Maximum number of colors supported by the context.
    , context_color_count :: Int
    -- | if the cursor can be shown / hidden
    , supports_cursor_visibility :: Bool
    , assumed_state_ref :: IORef AssumedState
    -- | Acquire display access to the given region of the display.
    -- Currently all regions have the upper left corner of (0,0) and the lower right corner at 
    -- (max display_width provided_width, max display_height provided_height)
    , mk_display_context :: MonadIO m => Output -> DisplayRegion -> m DisplayContext
    }

display_context :: MonadIO m => Output -> DisplayRegion -> m DisplayContext
display_context t = liftIO . mk_display_context t t

data AssumedState = AssumedState
    { prev_fattr :: Maybe FixedAttr
    , prev_output_ops :: Maybe DisplayOps
    }

initial_assumed_state :: AssumedState
initial_assumed_state = AssumedState Nothing Nothing

data DisplayContext = DisplayContext
    { context_device :: Output
    -- | Provide the bounds of the display context. 
    , context_region :: DisplayRegion
    --  | sets the output position to the specified row and column. Where the number of bytes
    --  required for the control codes can be specified seperate from the actual byte sequence.
    , write_move_cursor :: Int -> Int -> Write
    , write_show_cursor :: Write
    , write_hide_cursor :: Write
    --  | Assure the specified output attributes will be applied to all the following text until the
    --  next output attribute change. Where the number of bytes required for the control codes can
    --  be specified seperate from the actual byte sequence.  The required number of bytes must be
    --  at least the maximum number of bytes required by any attribute changes.  The serialization
    --  equations must provide the ptr to the next byte to be specified in the output buffer.
    --
    --  The currently applied display attributes are provided as well. The Attr data type can
    --  specify the style or color should not be changed from the currently applied display
    --  attributes. In order to support this the currently applied display attributes are required.
    --  In addition it may be possible to optimize the state changes based off the currently applied
    --  display attributes.
    , write_set_attr :: FixedAttr -> Attr -> DisplayAttrDiff -> Write
    -- | Reset the display attributes to the default display attributes
    , write_default_attr :: Write
    , write_row_end :: Write
    -- | See Graphics.Vty.Output.XTermColor.inline_hack
    , inline_hack :: IO ()
    }

-- | All terminals serialize UTF8 text to the terminal device exactly as serialized in memory.
write_utf8_text  :: BS.ByteString -> Write
write_utf8_text str dest_ptr = writeByteString

-- | Displays the given `Picture`.
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
output_picture :: MonadIO m => DisplayContext -> Picture -> m ()
output_picture dc pic = liftIO $ do
    as <- readIORef (assumed_state_ref $ context_device dc)
    let manip_cursor = supports_cursor_visibility (context_device dc)
    let !r = context_region dc
        !ops = display_ops_for_pic pic r
        !initial_attr = FixedAttr default_style_mask Nothing Nothing
        -- Diff the previous output against the requested output. Differences are currently on a per-row
        -- basis.
        -- \todo handle resizes that crop the dominate directions better.
        diffs :: [Bool] = case prev_output_ops as of
            Nothing -> replicate (fromEnum $ region_height $ effected_region ops) True
            Just previous_ops -> if effected_region previous_ops /= effected_region ops
                then replicate (display_ops_rows ops) True
                else zipWith (/=) (Vector.toList previous_ops)
                                  (Vector.toList ops)
        -- determine the number of bytes required to completely serialize the output ops.
        total =   (if manip_cursor then hide_cursor_required_bytes dc else 0)
                + default_attr_required_bytes dc
                + required_bytes dc initial_attr diffs ops 
                + case pic_cursor pic of
                    _ | not manip_cursor -> 0
                    NoCursor -> 0
                    Cursor x y ->
                        let { m = cursor_output_map ops $ pic_cursor pic; (ox, oy) = char_to_output_pos m ( x, y ) }
		                in show_cursor_required_bytes dc + move_cursor_required_bytes dc ox oy
    -- ... then serialize
    allocaBytes (fromEnum total) $ \start_ptr -> do
        ptr <- case manip_cursor of
                True  -> serialize_hide_cursor dc start_ptr
                False -> return start_ptr
        ptr' <- serialize_default_attr dc ptr
        ptr'' <- serialize_output_ops dc ptr' initial_attr diffs ops
        end_ptr <- case pic_cursor pic of
                        _ | not manip_cursor -> return ptr''
                        NoCursor -> return ptr''
                        Cursor x y -> do
                            let m = cursor_output_map ops $ pic_cursor pic
                                (ox, oy) = char_to_output_pos m (x,y)
                            serialize_show_cursor dc ptr'' >>= serialize_move_cursor dc ox oy
        -- todo: How to handle exceptions?
        -- probably set the prev_output_ops to Nothing
        case end_ptr `minusPtr` start_ptr of
            count | count < 0 -> fail "End pointer before start of buffer."
                  | toEnum count > total -> fail $ "End pointer past end of buffer by " ++ show (toEnum count - total)
                  | otherwise -> output_byte_buffer (context_device dc) start_ptr (toEnum count)
        -- Cache the output spans.
        let as' = as { prev_output_ops = Just ops }
        writeIORef (assumed_state_ref $ context_device dc) as'

required_bytes :: DisplayContext -> FixedAttr -> [Bool] -> DisplayOps -> Int
required_bytes dc in_fattr diffs ops = 
    let (_, n, _, _) = Vector.foldl' required_bytes' (0, 0, in_fattr, diffs) ops
    in n
    where 
        required_bytes' (y, current_sum, fattr, True : diffs') span_ops
            =   let (s, fattr') = span_ops_required_bytes dc y fattr span_ops 
                in ( y + 1, s + current_sum, fattr', diffs' )
        required_bytes' (y, current_sum, fattr, False : diffs') _span_ops
            = ( y + 1, current_sum, fattr, diffs' )
        required_bytes' (_y, _current_sum, _fattr, [] ) _span_ops
            = error "shouldn't be possible"

span_ops_required_bytes :: DisplayContext -> Int -> FixedAttr -> SpanOps -> (Int, FixedAttr)
span_ops_required_bytes dc y in_fattr span_ops =
    -- The first operation is to set the cursor to the start of the row
    let header_required_bytes = move_cursor_required_bytes dc 0 y
    -- then the span ops are serialized in the order specified
    in Vector.foldl' ( \(current_sum, fattr) op -> let (c, fattr') = span_op_required_bytes dc fattr op 
                                                   in (c + current_sum, fattr') 
                     ) 
                     (header_required_bytes, in_fattr)
                     span_ops

span_op_required_bytes :: DisplayContext -> FixedAttr -> SpanOp -> (Int, FixedAttr)
span_op_required_bytes dc fattr (TextSpan attr _ _ str) =
    let attr' = limit_attr_for_display (context_device dc) attr
        diffs = display_attr_diffs fattr fattr'
        c = attr_required_bytes dc fattr attr' diffs
        fattr' = fix_display_attr fattr attr'
    in (c + utf8_text_required_bytes (T.encodeUtf8 $ TL.toStrict str), fattr')
span_op_required_bytes _dc _fattr (Skip _) = error "span_op_required_bytes for Skip."
span_op_required_bytes dc fattr (RowEnd _) = (row_end_required_bytes dc, fattr)

serialize_output_ops :: DisplayContext
                        -> OutputBuffer 
                        -> FixedAttr 
                        -> [Bool]
                        -> DisplayOps 
                        -> IO OutputBuffer
serialize_output_ops dc start_ptr in_fattr diffs ops = do
    (_, end_ptr, _, _) <- Vector.foldM' serialize_output_ops' 
                              ( 0, start_ptr, in_fattr, diffs ) 
                              ops
    return end_ptr
    where 
        serialize_output_ops' ( y, out_ptr, fattr, True : diffs' ) span_ops
            =   serialize_span_ops dc y out_ptr fattr span_ops 
                >>= return . ( \(out_ptr', fattr') -> ( y + 1, out_ptr', fattr', diffs' ) )
        serialize_output_ops' ( y, out_ptr, fattr, False : diffs' ) _span_ops
            = return ( y + 1, out_ptr, fattr, diffs' )
        serialize_output_ops' (_y, _out_ptr, _fattr, [] ) _span_ops
            = error "vty - output spans without a corresponding diff."

serialize_span_ops :: DisplayContext
                      -> Int 
                      -> OutputBuffer 
                      -> FixedAttr 
                      -> SpanOps 
                      -> IO (OutputBuffer, FixedAttr)
serialize_span_ops dc y out_ptr in_fattr span_ops = do
    -- The first operation is to set the cursor to the start of the row
    out_ptr' <- serialize_move_cursor dc 0 y out_ptr
    -- then the span ops are serialized in the order specified
    Vector.foldM ( \(out_ptr'', fattr) op -> serialize_span_op dc op out_ptr'' fattr ) 
                 (out_ptr', in_fattr)
                 span_ops

-- | 
-- TODO: move this into the terminal implementation?
serialize_span_op :: DisplayContext
                     -> SpanOp 
                     -> OutputBuffer 
                     -> FixedAttr
                     -> IO (OutputBuffer, FixedAttr)
serialize_span_op dc (TextSpan attr _ _ str) out_ptr fattr = do
    let attr' = limit_attr_for_display (context_device dc) attr
        fattr' = fix_display_attr fattr attr'
        diffs = display_attr_diffs fattr fattr'
    out_ptr' <- serialize_set_attr dc fattr attr' diffs out_ptr
    out_ptr'' <- serialize_utf8_text (T.encodeUtf8 $ TL.toStrict str) out_ptr'
    return (out_ptr'', fattr')
serialize_span_op _dc (Skip _) _out_ptr _fattr = error "serialize_span_op for Skip"
serialize_span_op dc (RowEnd _) out_ptr fattr = do
    out_ptr' <- serialize_row_end dc out_ptr
    return (out_ptr', fattr)

send_to_terminal :: Output -> Int -> (Ptr Word8 -> IO (Ptr Word8)) -> IO ()
send_to_terminal t c f = allocaBytes (fromEnum c) $ \start_ptr -> do
    end_ptr <- f start_ptr
    case end_ptr `minusPtr` start_ptr of
        count | count < 0 -> fail "End pointer before start pointer."
              | toEnum count > c -> fail $ "End pointer past end of buffer by " ++ show (toEnum count - c)
              | otherwise -> output_byte_buffer t start_ptr (toEnum count)

-- | The cursor position is given in X,Y character offsets. Due to multi-column characters this
-- needs to be translated to column, row positions.
data CursorOutputMap = CursorOutputMap
    { char_to_output_pos :: (Int, Int) -> (Int, Int)
    } 

cursor_output_map :: DisplayOps -> Cursor -> CursorOutputMap
cursor_output_map span_ops _cursor = CursorOutputMap
    { char_to_output_pos = \(cx, cy) -> (cursor_column_offset span_ops cx cy, cy)
    }

cursor_column_offset :: DisplayOps -> Int -> Int -> Int
cursor_column_offset ops cx cy =
    let cursor_row_ops = Vector.unsafeIndex ops (fromEnum cy)
        (out_offset, _, _) 
            = Vector.foldl' ( \(d, current_cx, done) op -> 
                        if done then (d, current_cx, done) else case span_op_has_width op of
                            Nothing -> (d, current_cx, False)
                            Just (cw, ow) -> case compare cx (current_cx + cw) of
                                    GT -> ( d + ow
                                          , current_cx + cw
                                          , False 
                                          )
                                    EQ -> ( d + ow
                                          , current_cx + cw
                                          , True 
                                          )
                                    LT -> ( d + columns_to_char_offset (cx - current_cx) op
                                          , current_cx + cw
                                          , True
                                          )
                      )
                      (0, 0, False)
                      cursor_row_ops
    in out_offset

-- | Not all terminals support all display attributes. This filters a display attribute to what the
-- given terminal can display.
limit_attr_for_display :: Output -> Attr -> Attr
limit_attr_for_display t attr 
    = attr { attr_fore_color = clamp_color $ attr_fore_color attr
           , attr_back_color = clamp_color $ attr_back_color attr
           }
    where
        clamp_color Default     = Default
        clamp_color KeepCurrent = KeepCurrent
        clamp_color (SetTo c)   = clamp_color' c
        clamp_color' (ISOColor v) 
            | context_color_count t < 8            = Default
            | context_color_count t < 16 && v >= 8 = SetTo $ ISOColor (v - 8)
            | otherwise                            = SetTo $ ISOColor v
        clamp_color' (Color240 v)
            -- TODO: Choose closes ISO color?
            | context_color_count t < 8            = Default
            | context_color_count t < 16           = Default
            | context_color_count t == 240         = SetTo $ Color240 v
            | otherwise 
                = let p :: Double = fromIntegral v / 240.0 
                      v' = floor $ p * (fromIntegral $ context_color_count t)
                  in SetTo $ Color240 v'
