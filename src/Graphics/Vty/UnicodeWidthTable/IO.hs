{-# LANGUAGE CPP #-}
module Graphics.Vty.UnicodeWidthTable.IO
  ( readUnicodeWidthTable
  , writeUnicodeWidthTable
  )
where

import Control.Monad (when, forM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BSL
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif

import Graphics.Vty.UnicodeWidthTable.Types

-- | Load a binary unicode width table from the specified file.
--
-- This either returns a successfully parsed table or a table parsing
-- error message. This does not handle I/O exceptions.
readUnicodeWidthTable :: FilePath -> IO (Either String UnicodeWidthTable)
readUnicodeWidthTable path = do
    body <- BSL.readFile path
    case runGetOrFail tableParser body of
        Left (_, _, msg) ->
            return $ Left msg

        -- Even if we parsed a table, leftover bytes indicate something
        -- could be wrong.
        Right (remainingBytes, _, _) | not (BSL.null remainingBytes) ->
            return $ Left $ "Error: " <> show (BSL.length remainingBytes) <>
                            " byte(s) left unconsumed"

        Right (_, _, table) ->
            return $ Right table

-- | Write the unicode width table to the specified path.
--
-- This does not handle I/O exceptions.
writeUnicodeWidthTable :: FilePath -> UnicodeWidthTable -> IO ()
writeUnicodeWidthTable path table = do
    let body = runPut (tableV1Writer table)
    BSL.writeFile path body

-- | Width table magic bytes for use in the binary format.
widthTableMagic :: Word32
widthTableMagic = 0xc1a9f7e0

-- NB: This parsing code uses explicit types for parsing each value to
-- catch situations where someone changes the types of the fields of
-- WidthTableRange or other values and in doing so would silently break
-- the parser. If the parser used a style like
--
-- WidthTableRange <$> get <*> get <*> get
--
-- then that would result in valid Haskell code that would fail to read
-- files that it would have previously parsed. We want those errors
-- caught by GHc. At the time of this writing, the library data types
-- *happen* to match the binary layout of the version 1 format, but that
-- could definitely change in later formats. In those cases we should
-- deal with the difference by doing the appropriate conversions.

tableParser :: Get UnicodeWidthTable
tableParser = do
    magic :: Word32
          <- getWord32le

    when (magic /= widthTableMagic) $
        fail "Table magic number invalid"

    version :: Word8
            <- get

    case version of
        1 -> tableV1Parser
        _ -> fail "Table version invalid"

tableV1Parser :: Get UnicodeWidthTable
tableV1Parser = do
    numRanges :: Word32
              <- getWord32le

    let parseRange = do
            start :: Word32
                  <- getWord32le
            size :: Word32
                 <- getWord32le
            cols :: Word8
                 <- get
            return WidthTableRange { rangeStart = start
                                   , rangeSize = size
                                   , rangeColumns = cols
                                   }

    ranges <- forM [1..numRanges] $ const parseRange

    return UnicodeWidthTable { unicodeWidthTableRanges = ranges
                             }

tableV1Writer :: UnicodeWidthTable -> Put
tableV1Writer table = do
    -- Magic bytes
    putWord32le widthTableMagic

    -- Version
    putWord8 1

    -- Number of ranges
    let ranges = unicodeWidthTableRanges table
    let numRanges = length ranges
    putWord32le (fromIntegral numRanges)

    -- Ranges
    let putRange r = do
            putWord32le $ rangeStart r
            putWord32le $ rangeSize r
            putWord8 $ rangeColumns r

    mapM_ putRange ranges
