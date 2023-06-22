{-# LANGUAGE RecordWildCards, CPP #-}
-- | This module provides functions for accessing the current terminal
-- or a specific terminal device.
--
-- See also "Graphics.Vty.Output.Interface", which defines the generic
-- device output interface that all Vty platform implementations must
-- implement.
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
