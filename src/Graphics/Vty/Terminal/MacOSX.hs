-- Copyright Corey O'Connor
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
module Graphics.Vty.Terminal.MacOSX ( reserve_terminal )
    where

import Graphics.Vty.Terminal.Interface
import qualified Graphics.Vty.Terminal.TerminfoBased as TerminfoBased

import Control.Applicative
import Control.Monad.Trans

import System.IO

-- | for Terminal.app the terminal identifier "xterm" is used. For iTerm.app the terminal identifier
-- "xterm-256color" is used.
--
-- This effects the terminfo lookup.
reserve_terminal :: ( Applicative m, MonadIO m ) => String -> Handle -> m Terminal
reserve_terminal v out_handle = do
    let remap_term "iTerm.app" = "xterm-256color"
        remap_term _ = "xterm"
        flushed_put :: String -> IO ()
        flushed_put str = do
            hPutStr out_handle str
            hFlush out_handle
    t <- TerminfoBased.reserve_terminal (remap_term v) out_handle
    return $ t
        { terminal_ID = terminal_ID t ++ " (Mac)"
        , reserve_display = terminal_app_reserve_display flushed_put
        , release_display = terminal_app_release_display flushed_put
        }


-- | Terminal.app requires the xterm-color smcup and rmcup caps. Not the generic xterm ones.
-- Otherwise, Terminal.app expects the xterm caps.
smcup_str, rmcup_str :: String
smcup_str = "\ESC7\ESC[?47h"
rmcup_str = "\ESC[2J\ESC[?47l\ESC8"

-- | always smcup then clear the screen on terminal.app
--
-- \todo really?
terminal_app_reserve_display :: MonadIO m => (String -> IO ()) -> m ()
terminal_app_reserve_display flushed_put = liftIO $ do
    flushed_put smcup_str
    flushed_put clear_screen_str

terminal_app_release_display :: MonadIO m => (String -> IO ()) -> m ()
terminal_app_release_display flushed_put = liftIO $ do
    flushed_put rmcup_str

-- | iTerm needs a clear screen after smcup as well.
--
-- \todo but we apply to all mac terminals?
clear_screen_str :: String
clear_screen_str = "\ESC[H\ESC[2J"

