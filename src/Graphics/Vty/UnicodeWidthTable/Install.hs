{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Vty.UnicodeWidthTable.Install
  ( installUnicodeWidthTable
  , isCustomTableReady
  )
where

import Control.Monad (when, forM_)
import Data.Word (Word8, Word32)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif

import Graphics.Vty.UnicodeWidthTable.Types

-- | Returns True if and only if a custom table has been allocated and
-- marked as ready for use.
--
-- This function is not thread-safe.
isCustomTableReady :: IO Bool
isCustomTableReady = (== 1) <$> c_isCustomTableReady

-- This is the size of the allocated custom character width table, in
-- character slots. It's important that this be large enough to hold all
-- possible Unicode character values. At the time of this writing, the
-- valid Unicode range is 0 - 0x10ffff, hence this value.
tableSize :: Int
tableSize = 0x110000

-- | Install a custom unicode character width
-- table. Such tables are obtained with
-- 'Graphics.Vty.UnicodeWidthTable.Query.buildUnicodeWidthTable' and
-- 'Graphics.Vty.UnicodeWidthTable.IO.readUnicodeWidthTable'.
--
-- ALERT! This function is probably not what you want to use because
-- it is automatically called by 'Graphics.Vty.mkVty'. You will only
-- ever need to call this function if you want to use functions
-- in 'Graphics.Text.Width' without controlling the terminal with
-- 'Graphics.Vty.mkVty'.
--
-- This affects the behavior of the 'Graphics.Vty.Image.wcwidth'
-- function and functions that call it. It does so by
-- changing global state available to the C implementation
-- of 'Graphics.Vty.Image.wcwidth'. To ensure that your
-- program gets consistent results from evaluating calls to
-- 'Graphics.Vty.Image.wcwidth', the installation of a custom table
-- should be performed before you call 'Graphics.Vty.Image.wcwidth' in
-- your program.
--
-- This is best done at Vty startup, and if you use
-- 'Graphics.Vty.mkVty', that function calls this automatically based on
-- the Vty configuration's declared width tables. It is exposed as part
-- of the public API so that applications can call this as needed if
-- they don't want to control the terminal with 'mkVty' but do want to
-- make calls to 'Graphics.Vty.Image.wcwidth'.
--
-- It's also important to note that once a custom table has been
-- installed, it is permanent for the life of the process. No new table
-- can be installed, and the new custom table cannot be removed.
--
-- If this function fails for any reason -- if the table cannot be
-- installed or is invalid, or if a custom table already exists -- this
-- will raise an exception by calling 'error'.
--
-- This function is not thread-safe.
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

        when (result /= 0) $ do
            deallocateCustomTable
            error $ "installUnicodeWidthTable: error installing range " <>
                    show r <> ", status " <> show result

    actResult <- activateCustomTable
    when (actResult /= 0) $
        error $ "installUnicodeWidthTable: error activating custom " <>
                "table, status " <> show actResult

------------------------------------------------------------------------
-- C imports

foreign import ccall unsafe "vty_init_custom_table"
    initCustomTable :: Int -> IO Int

foreign import ccall unsafe "vty_set_custom_table_range"
    setCustomTableRange :: Word32 -> Word32 -> Word8 -> IO Int

foreign import ccall unsafe "vty_activate_custom_table"
    activateCustomTable :: IO Int

foreign import ccall unsafe "vty_custom_table_ready"
    c_isCustomTableReady :: IO Int

foreign import ccall unsafe "vty_deallocate_custom_table"
    deallocateCustomTable :: IO ()
