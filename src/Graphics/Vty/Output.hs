{-# LANGUAGE RecordWildCards, CPP #-}
-- | This module provides functions for accessing the current terminal
-- or a specific terminal device.
--
-- See also:
--
-- 1. "Graphics.Vty.Output": This instantiates an abtract interface
-- to the terminal based on the @TERM@ and @COLORTERM@ environment
-- variables.
--
-- 2. "Graphics.Vty.Output.Interface": Defines the generic interface all
-- terminal modules need to implement.
--
-- 3. "Graphics.Vty.Output.TerminfoBased": Defines a terminal instance
-- that uses terminfo for all control strings. No attempt is made to
-- change the character set to UTF-8 for these terminals.
--
-- 4. "Graphics.Vty.Output.XTermColor": This module contains an
-- interface suitable for xterm-like terminals. These are the terminals
-- where @TERM@ begins with @xterm@. This does use terminfo for as many
-- control codes as possible.
module Graphics.Vty.Output
  ( setCursorPos
  , hideCursor
  , showCursor
  )
where

import Control.Monad (when)

import Graphics.Vty.Image (regionWidth, regionHeight)
import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder (writeToByteString)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | Sets the cursor position to the given output column and row.
--
-- This is not necessarily the same as the character position with the
-- same coordinates. Characters can be a variable number of columns in
-- width.
--
-- Currently, the only way to set the cursor position to a given
-- character coordinate is to specify the coordinate in the Picture
-- instance provided to 'outputPicture' or 'refresh'.
setCursorPos :: Output -> Int -> Int -> IO ()
setCursorPos t x y = do
    bounds <- displayBounds t
    when (x >= 0 && x < regionWidth bounds && y >= 0 && y < regionHeight bounds) $ do
        dc <- displayContext t bounds
        outputByteBuffer t $ writeToByteString $ writeMoveCursor dc x y

-- | Hides the cursor.
hideCursor :: Output -> IO ()
hideCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    outputByteBuffer t $ writeToByteString $ writeHideCursor dc

-- | Shows the cursor.
showCursor :: Output -> IO ()
showCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    outputByteBuffer t $ writeToByteString $ writeShowCursor dc
