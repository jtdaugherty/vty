{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Output functions.
--
-- Access to the current terminal or a specific terminal device.
--
-- See also:
--
-- 1. "Graphics.Vty.Output": This instantiates an abtract interface to
-- the terminal interface based on the TERM and COLORTERM environment
-- variables.
--
-- 2. "Graphics.Vty.Output.Interface": Defines the generic interface all
-- terminals need to implement.
--
-- 3. "Graphics.Vty.Output.TerminfoBased": Defines a terminal instance
-- that uses terminfo for all control strings. No attempt is made to
-- change the character set to UTF-8 for these terminals. I don't know a
-- way to reliably determine if that is required or how to do so.
--
-- 4. "Graphics.Vty.Output.XTermColor": This module contains an
-- interface suitable for xterm-like terminals. These are the terminals
-- where TERM == xterm. This does use terminfo for as many control codes
-- as possible.
module Graphics.Vty.Output
  ( outputForConfig
  , setCursorPos
  , hideCursor
  , showCursor
  )
where

import Control.Monad (when)

import Graphics.Vty.Config
import Graphics.Vty.Image (regionWidth, regionHeight)
import Graphics.Vty.Output.Interface
import Graphics.Vty.Output.XTermColor as XTermColor
import Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Blaze.ByteString.Builder (writeToByteString)

import Control.Monad.Trans

import Data.List (isPrefixOf)
import Data.Monoid ((<>))

-- | Returns a `Output` for the terminal specified in `Config`
--
-- The specific Output implementation used is hidden from the API user.
-- All terminal implementations are assumed to perform more, or less,
-- the same. Currently, all implementations use terminfo for at least
-- some terminal specific information.
--
-- Specifics about it being based on terminfo are hidden from the API
-- user. If a terminal implementation is developed for a terminal
-- without terminfo support then Vty should work as expected on that
-- terminal.
--
-- Selection of a terminal is done as follows:
--
--      * If TERM contains "xterm" or "screen", use XTermColor.
--      * otherwise use the TerminfoBased driver.
outputForConfig :: Config -> IO Output
outputForConfig Config{ outputFd = Just fd, termName = Just termName, .. } = do
    t <- if "xterm" `isPrefixOf` termName || "screen" `isPrefixOf` termName
        then XTermColor.reserveTerminal termName fd
        -- Not an xterm-like terminal. try for generic terminfo.
        else TerminfoBased.reserveTerminal termName fd

    case mouseMode of
        Just s -> setMode t Mouse s
        Nothing -> return ()

    case bracketedPasteMode of
        Just s -> setMode t BracketedPaste s
        Nothing -> return ()

    return t
outputForConfig config = (<> config) <$> standardIOConfig >>= outputForConfig

-- | Sets the cursor position to the given output column and row.
--
-- This is not necessarially the same as the character position with the
-- same coordinates. Characters can be a variable number of columns in
-- width.
--
-- Currently, the only way to set the cursor position to a given
-- character coordinate is to specify the coordinate in the Picture
-- instance provided to outputPicture or refresh.
setCursorPos :: MonadIO m => Output -> Int -> Int -> m ()
setCursorPos t x y = do
    bounds <- displayBounds t
    when (x >= 0 && x < regionWidth bounds && y >= 0 && y < regionHeight bounds) $ do
        dc <- displayContext t bounds
        liftIO $ outputByteBuffer t $ writeToByteString $ writeMoveCursor dc x y

-- | Hides the cursor
hideCursor :: MonadIO m => Output -> m ()
hideCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    liftIO $ outputByteBuffer t $ writeToByteString $ writeHideCursor dc

-- | Shows the cursor
showCursor :: MonadIO m => Output -> m ()
showCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    liftIO $ outputByteBuffer t $ writeToByteString $ writeShowCursor dc
