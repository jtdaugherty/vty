{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
--  | Terminal Output.
--
--  Output access to the current terminal or a specific terminal device.
--
--  See also:
--
--  1. "Graphics.Vty.Output": This instantiates an abtract interface to the terminal interface based
--  on the TERM and COLORTERM environment variables.
--
--  2. "Graphics.Vty.Output.Interface": Defines the generic interface all terminals need to implement.
--
--  3. "Graphics.Vty.Output.Terminfo": Defines a terminal instance that uses terminfo for all
--  control strings.  No attempt is made to change the character set to UTF-8 for these terminals.
--
--  coconnor: I don't know a way to reliably determine if that is required or how to do so.
--
--  4. "Graphics.Vty.Output.XTermColor": This module contains an interface suitable for xterm-like
--  terminals. These are the terminals where TERM == xterm. This does use terminfo for as many
--  control codes as possible.
module Graphics.Vty.Output ( module Graphics.Vty.Output
                           , Output(..) -- \todo hide constructors
                           , DisplayContext(..)
                           , AssumedState(..)
                           , outputPicture
                           , outputPictureToContext
                           , outputForConfig
                           )
    where

import Graphics.Vty.Prelude

import Graphics.Vty.Output.Interface

#ifdef TERMINFO
import Graphics.Vty.Output.Terminfo
#endif

#ifdef WINDOWS
import Graphics.Vty.Output.Windows
#endif

import Control.Monad.Operational
import Control.Monad.Trans

-- | Sets the cursor position to the given output column and row.
--
-- This is not necessarially the same as the character position with the same coordinates.
-- Characters can be a variable number of columns in width.
--
-- Currently, the only way to set the cursor position to a given character coordinate is to specify
-- the coordinate in the Picture instance provided to outputPicture or refresh.
setCursorPos :: MonadIO m => Output -> Int -> Int -> m ()
setCursorPos t x y = do
    bounds <- displayBounds t
    when (x >= 0 && x < regionWidth bounds && y >= 0 && y < regionHeight bounds) $ do
        outputDisplayCommands t $ singleton $ MoveCursor x y

-- | Hides the cursor
hideCursor :: MonadIO m => Output -> m ()
hideCursor t = outputDisplayCommands t $ singleton $ HideCursor

-- | Shows the cursor
showCursor :: MonadIO m => Output -> m ()
showCursor t = outputDisplayCommands t $ singleton $ ShowCursor
