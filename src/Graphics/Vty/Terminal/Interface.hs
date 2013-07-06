-- Copyright Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Vty.Terminal.Interface ( module Graphics.Vty.Terminal.Interface
                                       , OutputBuffer
                                       )
    where

import Data.Marshalling

import Graphics.Vty.Picture
import Graphics.Vty.Span
import Graphics.Vty.DisplayRegion

import Graphics.Vty.DisplayAttributes

import Control.Monad ( liftM )
import Control.Monad.Trans

import qualified Data.ByteString.Internal as BS
import Data.IORef
import qualified Data.Vector as Vector 

import System.IO

-- | An handle to a terminal that hides the implementation.
data TerminalHandle where
    TerminalHandle :: Terminal t => t -> IORef TerminalState -> TerminalHandle

state_ref :: TerminalHandle -> IORef TerminalState
state_ref (TerminalHandle _ s_ref) = s_ref

new_terminal_handle :: forall m t. ( MonadIO m, Terminal t ) => t -> m TerminalHandle
new_terminal_handle t = do
    s_ref <- liftIO $ newIORef initial_terminal_state
    return $ TerminalHandle t s_ref

-- | The current terminal state. This may not exactly be known.
data TerminalState = TerminalState
    { known_fattr :: Maybe FixedAttr
    }

-- | Initially we know nothing about a terminal's state.
initial_terminal_state :: TerminalState
initial_terminal_state = TerminalState Nothing

class Terminal t where
    -- | Text identifier for the terminal. Used for debugging.
    terminal_ID :: t -> String
    -- | 
    release_terminal :: MonadIO m => t -> m ()
    -- | Clear the display and initialize the terminal to some initial display state. 
    --
    -- The expectation of a program is that the display starts in some initial state. 
    -- The initial state would consist of fixed values:
    --  - cursor at top left
    --  - UTF-8 character encoding
    --  - drawing characteristics are the default
    -- The abstract operation I think all these behaviors are instances of is reserving exclusive
    -- access to a display such that:
    --  - The previous state cannot be determined
    --  - When exclusive access to a display is release the display returns to the previous state.
    reserve_display :: MonadIO m => t -> m ()

    -- | Return the display to the state before reserve_display
    -- If no previous state then set the display state to the initial state.
    release_display :: MonadIO m => t -> m ()
    
    -- | Returns the current display bounds.
    display_bounds :: MonadIO m => t -> m DisplayRegion

    -- Internal method used to provide the DisplayTerminal instance to the DisplayHandle
    -- constructor.
    display_terminal_instance :: MonadIO m 
                              => t 
                              -> DisplayRegion 
                              -> (forall d. DisplayTerminal d => d -> DisplayHandle) 
                              -> m DisplayHandle

    -- | Output the byte buffer of the specified size to the terminal device.  The size is equal to
    -- end_ptr - start_ptr
    output_byte_buffer :: t -> OutputBuffer -> Int -> IO ()

    -- | Handle of output device
    output_handle :: t -> IO Handle

instance Terminal TerminalHandle where
    terminal_ID (TerminalHandle t _) = terminal_ID t
    release_terminal (TerminalHandle t _) = release_terminal t
    reserve_display (TerminalHandle t _) = reserve_display t
    release_display (TerminalHandle t _) = release_display t
    display_bounds (TerminalHandle t _) = display_bounds t
    display_terminal_instance (TerminalHandle t _) = display_terminal_instance t
    output_byte_buffer (TerminalHandle t _) = output_byte_buffer t
    output_handle (TerminalHandle t _) = output_handle t

data DisplayHandle where
    DisplayHandle :: forall d . DisplayTerminal d => d -> TerminalHandle -> DisplayState -> DisplayHandle

-- | Acquire display access to the given region of the display.
-- Currently all regions have the upper left corner of (0,0) and the lower right corner at 
-- (max display_width provided_width, max display_height provided_height)
display_context :: MonadIO m => TerminalHandle -> DisplayRegion -> m DisplayHandle
display_context t b = do
    s <- initial_display_state
    display_terminal_instance t b (\ d -> DisplayHandle d t s)

data DisplayState = DisplayState
    { previous_output_ref :: IORef (Maybe DisplayOps)
    }

initial_display_state :: MonadIO m => m DisplayState
initial_display_state = liftM DisplayState $ liftIO $ newIORef Nothing

class DisplayTerminal d where
    -- | Provide the bounds of the display context. 
    context_region :: d -> DisplayRegion

    -- | Maximum number of colors supported by the context.
    context_color_count :: d -> Int

    --  | sets the output position to the specified row and column. Where the number of bytes
    --  required for the control codes can be specified seperate from the actual byte sequence.
    move_cursor_required_bytes :: d -> Int -> Int -> Int
    serialize_move_cursor :: MonadIO m => d -> Int -> Int -> OutputBuffer -> m OutputBuffer

    show_cursor_required_bytes :: d -> Int
    serialize_show_cursor :: MonadIO m => d -> OutputBuffer -> m OutputBuffer

    hide_cursor_required_bytes :: d -> Int
    serialize_hide_cursor :: MonadIO m => d -> OutputBuffer -> m OutputBuffer

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
    attr_required_bytes :: d -> FixedAttr -> Attr -> DisplayAttrDiff -> Int
    serialize_set_attr :: MonadIO m => d -> FixedAttr -> Attr -> DisplayAttrDiff -> OutputBuffer -> m OutputBuffer

    -- | Reset the display attributes to the default display attributes
    default_attr_required_bytes :: d -> Int
    serialize_default_attr :: MonadIO m => d -> OutputBuffer -> m OutputBuffer

    -- | See Graphics.Vty.Terminal.XTermColor.inline_hack
    inline_hack :: MonadIO m => d -> m ()
    inline_hack _d = return ()


instance DisplayTerminal DisplayHandle where
    context_region (DisplayHandle d _ _) = context_region d
    context_color_count (DisplayHandle d _ _) = context_color_count d
    move_cursor_required_bytes (DisplayHandle d _ _) = move_cursor_required_bytes d
    serialize_move_cursor (DisplayHandle d _ _) = serialize_move_cursor d
    show_cursor_required_bytes (DisplayHandle d _ _) = show_cursor_required_bytes d
    serialize_show_cursor (DisplayHandle d _ _) = serialize_show_cursor d
    hide_cursor_required_bytes (DisplayHandle d _ _) = hide_cursor_required_bytes d
    serialize_hide_cursor (DisplayHandle d _ _) = serialize_hide_cursor d
    attr_required_bytes (DisplayHandle d _ _) = attr_required_bytes d
    serialize_set_attr (DisplayHandle d _ _) = serialize_set_attr d
    default_attr_required_bytes (DisplayHandle d _ _) = default_attr_required_bytes d
    serialize_default_attr (DisplayHandle d _ _) = serialize_default_attr d
    inline_hack (DisplayHandle d _ _) = inline_hack d 
    
-- | All terminals serialize UTF8 text to the terminal device exactly as serialized in memory.
utf8_text_required_bytes ::  BS.ByteString -> Int
utf8_text_required_bytes str =
    let (_, _, src_bytes_length) = BS.toForeignPtr str
    in src_bytes_length

-- | All terminals serialize UTF8 text to the terminal device exactly as serialized in memory.
serialize_utf8_text  :: MonadIO m => BS.ByteString -> OutputBuffer -> m OutputBuffer
serialize_utf8_text str dest_ptr =
    let (src_fptr, src_ptr_offset, src_bytes_length) = BS.toForeignPtr str
    in liftIO $ withForeignPtr src_fptr $ \src_ptr -> do
        let src_ptr' = src_ptr `plusPtr` src_ptr_offset
        BS.memcpy dest_ptr src_ptr' (toEnum src_bytes_length) 
        return (dest_ptr `plusPtr` src_bytes_length)


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
-- 
-- todo: specify possible IO exceptions.
-- abstract from IO monad to a MonadIO instance.
output_picture :: MonadIO m => DisplayHandle -> Picture -> m ()
output_picture (DisplayHandle d t s) pic = do
    let !r = context_region d
    let !ops = spans_for_pic pic r
    let !initial_attr = FixedAttr default_style_mask Nothing Nothing
    -- Diff the previous output against the requested output. Differences are currently on a per-row
    -- basis.
    diffs :: [Bool]
        <- liftIO ( readIORef (previous_output_ref s) )
            >>= \mprevious_ops -> case mprevious_ops of
                Nothing      
                    -> return $ replicate ( fromEnum $ region_height $ effected_region ops ) 
                                                   True
                Just previous_ops 
                    -> if effected_region previous_ops /= effected_region ops
                            then return $ replicate ( fromEnum $ region_height $ effected_region ops ) 
                                                    True
                            else return $ zipWith (/=) ( Vector.toList $ display_ops previous_ops ) 
                                                       ( Vector.toList $ display_ops ops )

    -- determine the number of bytes required to completely serialize the output ops.
    let total =   hide_cursor_required_bytes d 
                + default_attr_required_bytes d
                + required_bytes d initial_attr diffs ops 
                + case pic_cursor pic of
                    NoCursor -> 0
                    Cursor x y -> let m = cursor_output_map ops $ pic_cursor pic
		                      ( ox, oy ) = char_to_output_pos m ( x, y )
		                   in show_cursor_required_bytes d
                                      + move_cursor_required_bytes d ox oy

    -- ... then serialize
    liftIO $ allocaBytes (fromEnum total) $ \start_ptr -> do
        ptr <- serialize_hide_cursor d start_ptr
        ptr' <- serialize_default_attr d ptr
        ptr'' <- serialize_output_ops d ptr' initial_attr diffs ops
        end_ptr <- case pic_cursor pic of
                        NoCursor -> return ptr''
                        Cursor x y -> do
                            let m = cursor_output_map ops $ pic_cursor pic
                                (ox, oy) = char_to_output_pos m (x,y)
                            serialize_show_cursor d ptr'' >>= serialize_move_cursor d ox oy
        -- todo: How to handle exceptions?
        case end_ptr `minusPtr` start_ptr of
            count | count < 0 -> fail "End pointer before start of buffer."
                  | toEnum count > total -> fail $ "End pointer past end of buffer by " ++ show (toEnum count - total)
                  | otherwise -> output_byte_buffer t start_ptr (toEnum count)
        -- Cache the output spans.
        liftIO $ writeIORef (previous_output_ref s) (Just ops)
    return ()

required_bytes :: DisplayTerminal d => d -> FixedAttr -> [Bool] -> DisplayOps -> Int
required_bytes d in_fattr diffs ops = 
    let (_, n, _, _) = Vector.foldl' required_bytes' (0, 0, in_fattr, diffs) ( display_ops ops )
    in n
    where 
        required_bytes' (y, current_sum, fattr, True : diffs') span_ops
            =   let (s, fattr') = span_ops_required_bytes d y fattr span_ops 
                in ( y + 1, s + current_sum, fattr', diffs' )
        required_bytes' (y, current_sum, fattr, False : diffs') _span_ops
            = ( y + 1, current_sum, fattr, diffs' )
        required_bytes' (_y, _current_sum, _fattr, [] ) _span_ops
            = error "shouldn't be possible"

span_ops_required_bytes :: DisplayTerminal d => d -> Int -> FixedAttr -> SpanOps -> (Int, FixedAttr)
span_ops_required_bytes d y in_fattr span_ops = 
    -- The first operation is to set the cursor to the start of the row
    let header_required_bytes = move_cursor_required_bytes d 0 y
    -- then the span ops are serialized in the order specified
    in Vector.foldl' ( \(current_sum, fattr) op -> let (c, fattr') = span_op_required_bytes d fattr op 
                                                   in (c + current_sum, fattr') 
                     ) 
                     (header_required_bytes, in_fattr)
                     span_ops

span_op_required_bytes :: DisplayTerminal d => d -> FixedAttr -> SpanOp -> (Int, FixedAttr)
span_op_required_bytes d fattr (AttributeChange attr) = 
    let attr' = limit_attr_for_display d attr
        diffs = display_attr_diffs fattr fattr'
        c = attr_required_bytes d fattr attr' diffs
        fattr' = fix_display_attr fattr attr'
    in (c, fattr')
span_op_required_bytes _d fattr (TextSpan _ _ str) = (utf8_text_required_bytes str, fattr)

serialize_output_ops :: ( MonadIO m, DisplayTerminal d )
                        => d 
                        -> OutputBuffer 
                        -> FixedAttr 
                        -> [Bool]
                        -> DisplayOps 
                        -> m OutputBuffer
serialize_output_ops d start_ptr in_fattr diffs ops = do
    (_, end_ptr, _, _) <- Vector.foldM' serialize_output_ops' 
                              ( 0, start_ptr, in_fattr, diffs ) 
                              ( display_ops ops )
    return end_ptr
    where 
        serialize_output_ops' ( y, out_ptr, fattr, True : diffs' ) span_ops
            =   serialize_span_ops d y out_ptr fattr span_ops 
                >>= return . ( \(out_ptr', fattr') -> ( y + 1, out_ptr', fattr', diffs' ) )
        serialize_output_ops' ( y, out_ptr, fattr, False : diffs' ) _span_ops
            = return ( y + 1, out_ptr, fattr, diffs' )
        serialize_output_ops' (_y, _out_ptr, _fattr, [] ) _span_ops
            = error "shouldn't be possible"

serialize_span_ops :: ( MonadIO m, DisplayTerminal d )
                      => d 
                      -> Int 
                      -> OutputBuffer 
                      -> FixedAttr 
                      -> SpanOps 
                      -> m (OutputBuffer, FixedAttr)
serialize_span_ops d y out_ptr in_fattr span_ops = do
    -- The first operation is to set the cursor to the start of the row
    out_ptr' <- serialize_move_cursor d 0 y out_ptr
    -- then the span ops are serialized in the order specified
    Vector.foldM ( \(out_ptr'', fattr) op -> serialize_span_op d op out_ptr'' fattr ) 
                 (out_ptr', in_fattr)
                 span_ops

serialize_span_op :: ( MonadIO m, DisplayTerminal d )
                     => d 
                     -> SpanOp 
                     -> OutputBuffer 
                     -> FixedAttr
                     -> m (OutputBuffer, FixedAttr)
serialize_span_op d (AttributeChange attr) out_ptr fattr = do
    let attr' = limit_attr_for_display d attr
        fattr' = fix_display_attr fattr attr'
        diffs = display_attr_diffs fattr fattr'
    out_ptr' <- serialize_set_attr d fattr attr' diffs out_ptr
    return (out_ptr', fattr')
serialize_span_op _d (TextSpan _ _ str) out_ptr fattr = do
    out_ptr' <- serialize_utf8_text str out_ptr
    return (out_ptr', fattr)

marshall_to_terminal :: ( Terminal t )
                     => t -> Int -> (Ptr Word8 -> IO (Ptr Word8)) -> IO ()
marshall_to_terminal t c f = do
    start_ptr <- mallocBytes (fromEnum c)
    -- 
    -- todo: capture exceptions?
    end_ptr <- f start_ptr
    case end_ptr `minusPtr` start_ptr of
        count | count < 0 -> fail "End pointer before start pointer."
              | toEnum count > c -> fail $ "End pointer past end of buffer by " ++ show (toEnum count - c)
              | otherwise -> output_byte_buffer t start_ptr (toEnum count)
    free start_ptr
    return ()

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
cursor_column_offset span_ops cx cy =
    let cursor_row_ops = Vector.unsafeIndex (display_ops span_ops) (fromEnum cy)
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
limit_attr_for_display :: DisplayTerminal d => d -> Attr -> Attr
limit_attr_for_display d attr 
    = attr { attr_fore_color = clamp_color $ attr_fore_color attr
           , attr_back_color = clamp_color $ attr_back_color attr
           }
    where
        clamp_color Default     = Default
        clamp_color KeepCurrent = KeepCurrent
        clamp_color (SetTo c)   = clamp_color' c
        clamp_color' (ISOColor v) 
            | context_color_count d < 8            = Default
            | context_color_count d < 16 && v >= 8 = SetTo $ ISOColor (v - 8)
            | otherwise                            = SetTo $ ISOColor v
        clamp_color' (Color240 v)
            -- TODO: Choose closes ISO color?
            | context_color_count d < 8            = Default
            | context_color_count d < 16           = Default
            | context_color_count d == 240         = SetTo $ Color240 v
            | otherwise 
                = let p :: Double = fromIntegral v / 240.0 
                      v' = floor $ p * (fromIntegral $ context_color_count d)
                  in SetTo $ Color240 v'

