-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Vty.Terminal.XTermColor ( terminal_instance
                                        )
    where

import Graphics.Vty.Terminal.Interface
import qualified Graphics.Vty.Terminal.TerminfoBased as TerminfoBased

import Control.Applicative
import Control.Monad.Trans

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.IO

-- | An xterm color based terminal is some variant paired with a standard TerminfoBased terminal
-- interface.
--
-- For the most part, xterm terminals just use the TerminfoBased implementation.
data XTermColor = XTermColor 
    { xterm_variant :: String
    , super_term :: TerminalHandle
    }

-- | Initialize the display to UTF-8. 
terminal_instance :: ( Applicative m, MonadIO m ) => String -> Handle -> m XTermColor
terminal_instance variant out_handle = do
    -- If the terminal variant is xterm-color use xterm instead since, more often than not,
    -- xterm-color is broken.
    let variant' = if variant == "xterm-color" then "xterm" else variant
    flushed_put set_utf8_char_set
    t <- TerminfoBased.terminal_instance variant' out_handle >>= new_terminal_handle
    return $ XTermColor variant' t

-- | Output immediately followed by a flush.
--
-- \todo move out of this module.
flushed_put :: MonadIO m => String -> m ()
flushed_put str = do
    liftIO $ hPutStr stdout str
    liftIO $ hFlush stdout

-- | These sequences set xterm based terminals to UTF-8 output.
--
-- \todo I don't know of a terminfo cap that is equivalent to this.
set_utf8_char_set, set_default_char_set :: String
set_utf8_char_set = "\ESC%G"
set_default_char_set = "\ESC%@"

instance Terminal XTermColor where
    terminal_ID t = (show $ xterm_variant t) ++ " :: XTermColor"

    release_terminal t = do 
        flushed_put set_default_char_set
        release_terminal $ super_term t

    reserve_display t = reserve_display (super_term t)

    release_display t = release_display (super_term t)

    display_terminal_instance t b c = do
        d <- display_context (super_term t) b
        return $ c (DisplayContext d)

    display_bounds t = display_bounds (super_term t)
        
    output_byte_buffer t = output_byte_buffer (super_term t)

    output_handle t = output_handle (super_term t)

data DisplayContext = DisplayContext
    { super_display :: DisplayHandle
    }

instance DisplayTerminal DisplayContext where
    context_region d = context_region (super_display d)

    context_color_count d = context_color_count (super_display d)

    move_cursor_required_bytes d = move_cursor_required_bytes (super_display d)
    serialize_move_cursor d = serialize_move_cursor (super_display d)

    show_cursor_required_bytes d = show_cursor_required_bytes (super_display d)
    serialize_show_cursor d = serialize_show_cursor (super_display d)

    hide_cursor_required_bytes d = hide_cursor_required_bytes (super_display d)
    serialize_hide_cursor d = serialize_hide_cursor (super_display d)

    attr_required_bytes d = attr_required_bytes (super_display d)
    serialize_set_attr d = serialize_set_attr (super_display d)

    default_attr_required_bytes d = default_attr_required_bytes (super_display d)
    serialize_default_attr d = serialize_default_attr (super_display d)

    -- | I think xterm is broken: Reseting the background color as the first bytes serialized on a
    -- new line does not effect the background color xterm uses to clear the line. Which is used
    -- *after* the next newline.
    inline_hack d = do
        let t = case super_display d of
                    DisplayHandle _ t_ _ -> t_
        let s_utf8 = T.encodeUtf8 $ T.pack "\ESC[K"
        liftIO $ marshall_to_terminal t ( utf8_text_required_bytes s_utf8)
                                        ( serialize_utf8_text s_utf8 )

