-- Copyright 2009 Corey O'Connor
-- The standard Mac OS X terminals Terminal.app and iTerm both declare themselves to be
-- "xterm-color" by default. However the terminfo database for xterm-color included with OS X is
-- incomplete. 
--
-- This terminal implementation modifies the standard terminfo terminal as required for complete OS
-- X support.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Vty.Terminal.MacOSX ( terminal_instance
                                    )
    where

import Graphics.Vty.Terminal.Generic
import qualified Graphics.Vty.Terminal.TerminfoBased as TerminfoBased

import System.IO

data Term = Term 
    { super_term :: TerminalHandle
    }

-- Base this off the xterm terminfo database regardless of what the TERM environment variable is.
terminal_instance :: IO Term
terminal_instance = do
    t <- TerminfoBased.terminal_instance "xterm" >>= new_terminal_handle
    return $ Term t

flushed_put :: String -> IO ()
flushed_put str = do
    hPutStr stdout str
    hFlush stdout

instance Terminal Term where
    terminal_ID t = "Terminal.app :: MacOSX"

    release_terminal t = do 
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

