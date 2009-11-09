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

import Control.Applicative
import Control.Monad.Trans

import System.IO

data Term = Term 
    { super_term :: TerminalHandle
    }

-- for Terminal.app use "xterm". For iTerm.app use "xterm-256color"
terminal_instance :: ( Applicative m, MonadIO m ) => String -> m Term
terminal_instance v = do
    let base_term "iTerm.app" = "xterm-256color"
        base_term _ = "xterm"
    t <- TerminfoBased.terminal_instance (base_term v) >>= new_terminal_handle
    return $ Term t

flushed_put :: MonadIO m => String -> m ()
flushed_put str = do
    liftIO $ hPutStr stdout str
    liftIO $ hFlush stdout

-- Terminal.app really does want the xterm-color smcup and rmcup caps. Not the generic xterm ones.
smcup_str = "\ESC7\ESC[?47h"
rmcup_str = "\ESC[2J\ESC[?47l\ESC8"

instance Terminal Term where
    terminal_ID t = "Terminal.app :: MacOSX"

    release_terminal t = do 
        release_terminal $ super_term t

    reserve_display t = do
        flushed_put smcup_str

    release_display t = do
        flushed_put rmcup_str

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

