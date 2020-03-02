{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Vty.UnicodeWidthTable.Install
  ( installUnicodeWidthTable

  , isCustomTableReady
  )
where

import Control.Monad (when, forM_)
import Data.Word (Word8, Word32)

import Graphics.Vty.UnicodeWidthTable.Types

foreign import ccall unsafe "vty_init_custom_table"
    initCustomTable :: Int -> IO Int

foreign import ccall unsafe "vty_set_custom_table_range"
    setCustomTableRange :: Word32 -> Word32 -> Word8 -> IO Int

foreign import ccall unsafe "vty_activate_custom_table"
    activateCustomTable :: IO Int

-- | Returns 1 if and only if a custom table has been allocated and
-- marked as ready for use. Returns 0 otherwise.
foreign import ccall unsafe "vty_custom_table_ready"
    isCustomTableReady :: IO Int

-- This is the size of the allocated custom character width table, in
-- character slots. It's important that this be large enough to hold all
-- possible Unicode character values. At the time of this writing, the
-- valid Unicode range is 0 - 0x10ffff, hence this value.
tableSize :: Int
tableSize = 0x110000

-- | Install a custom unicode character width table.
--
-- This affects the behavior of the 'wcwidth' function and functions
-- that call it. It does so by changing global state available to the C
-- implementation of 'wcwidth'. To ensure that your program gets consistent
-- results from evaluating calls to 'wcwidth', the installation of a
-- custom table should be performed before you call 'wcwidth' in your
-- program. This is best done at Vty startup. It's also important to
-- note that once a custom table has been installed, it is permanent for
-- the life of the process. No new table can be installed, and the new
-- custom table cannot be removed.
--
-- If this function fails for any reason -- if the table cannot be
-- installed or is invalid, or if a custom table already exists -- this
-- will raise an exception by calling 'error'.
installUnicodeWidthTable :: UnicodeWidthTable -> IO ()
installUnicodeWidthTable table = do
    initResult <- initCustomTable tableSize
    when (initResult /= 0) $
        error $ "installUnicodeWidthTable: error initializing " <>
                "custom table, status " <> show initResult

    forM_ (unicodeWidthTableRanges table) $ \r -> do
        result <- setCustomTableRange (rangeStart r)
                                      (rangeSize r)
                                      (rangeColumns r)

        when (result /= 0) $
            error $ "installUnicodeWidthTable: error installing range " <>
                    show r <> ", status " <> show result

    actResult <- activateCustomTable
    when (actResult /= 0) $
        error $ "installUnicodeWidthTable: error activating custom " <>
                "table, status " <> show actResult
