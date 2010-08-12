-- Copyright 2009 Corey O'Connor
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Vty.Terminal.XTermColor ( terminal_instance
                                        )
    where

import Graphics.Vty.Terminal.Generic
import qualified Graphics.Vty.Terminal.TerminfoBased as TerminfoBased

import Control.Applicative
import Control.Monad.Trans

import qualified Data.String.UTF8 as UTF8

import System.IO

data XTermColor = XTermColor 
    { xterm_variant :: String
    , super_term :: TerminalHandle
    }

-- Initialize the display to UTF-8
-- Regardless of what is output the text encoding is assumed to be UTF-8
terminal_instance :: ( Applicative m, MonadIO m ) => String -> m XTermColor
terminal_instance variant = do
    -- If the terminal variant is xterm-color use xterm instead since, more often than not,
    -- xterm-color is broken.
    let variant' = if variant == "xterm-color" then "xterm" else variant
    flushed_put set_utf8_char_set
    t <- TerminfoBased.terminal_instance variant' >>= new_terminal_handle
    return $ XTermColor variant' t

flushed_put :: MonadIO m => String -> m ()
flushed_put str = do
    liftIO $ hPutStr stdout str
    liftIO $ hFlush stdout

-- Since I don't know of a terminfo string cap that produces these strings these are hardcoded.
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

    -- I think xterm is broken: Reseting the background color as the first bytes serialized on a new
    -- line does not effect the background color xterm uses to clear the line. Which is used *after*
    -- the next newline.
    inline_hack d = do
        let t = case super_display d of
                    DisplayHandle _ t_ _ -> t_
        let s_utf8 = UTF8.fromString "\ESC[K"
        liftIO $ marshall_to_terminal t ( utf8_text_required_bytes s_utf8)
                                        ( serialize_utf8_text s_utf8 )

