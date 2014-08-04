-- Copyright Corey O'Connor
-- The standard Mac OS X terminals Terminal.app and iTerm both declare themselves to be
-- "xterm-color" by default. However the terminfo database for xterm-color included with OS X is
-- incomplete. 
--
-- This terminal implementation modifies the standard terminfo terminal as required for complete OS
-- X support.
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Vty.Output.MacOSX ( reserveTerminal )
    where

import Graphics.Vty.Output.Interface
import qualified Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Control.Applicative
import Control.Monad (void)
import Control.Monad.Trans

import System.Posix.IO (fdWrite)
import System.Posix.Types (Fd)

-- | for Terminal.app the terminal identifier "xterm" is used. For iTerm.app the terminal identifier
-- "xterm-256color" is used.
--
-- This effects the terminfo lookup.
reserveTerminal :: ( Applicative m, MonadIO m ) => String -> Fd -> m Output
reserveTerminal v outFd = do
    let remapTerm "iTerm.app" = "xterm-256color"
        remapTerm _ = "xterm"
        flushedPut :: String -> IO ()
        flushedPut = void . fdWrite outFd
    t <- TerminfoBased.reserveTerminal (remapTerm v) outFd
    return $ t { terminalID = terminalID t ++ " (Mac)"
               , reserveDisplay = terminalAppReserveDisplay flushedPut
               , releaseDisplay = terminalAppReleaseDisplay flushedPut
               }

-- | Terminal.app requires the xterm-color smcup and rmcup caps. Not the generic xterm ones.
-- Otherwise, Terminal.app expects the xterm caps.
smcupStr, rmcupStr :: String
smcupStr = "\ESC7\ESC[?47h"
rmcupStr = "\ESC[2J\ESC[?47l\ESC8"

-- | always smcup then clear the screen on terminal.app
--
-- \todo really?
terminalAppReserveDisplay :: MonadIO m => (String -> IO ()) -> m ()
terminalAppReserveDisplay flushedPut = liftIO $ do
    flushedPut smcupStr
    flushedPut clearScreenStr

terminalAppReleaseDisplay :: MonadIO m => (String -> IO ()) -> m ()
terminalAppReleaseDisplay flushedPut = liftIO $ do
    flushedPut rmcupStr

-- | iTerm needs a clear screen after smcup as well.
--
-- \todo but we apply to all mac terminals?
clearScreenStr :: String
clearScreenStr = "\ESC[H\ESC[2J"

