{-# LANGUAGE CPP #-}
module Graphics.Vty.UnicodeWidthTable.IO
  ( readUnicodeWidthTable
  , parseUnicodeWidthTable
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
readUnicodeWidthTable path = parseUnicodeWidthTable <$> BSL.readFile path

-- | Parse a binary unicode width table.
parseUnicodeWidthTable :: BSL.ByteString -> Either String UnicodeWidthTable
parseUnicodeWidthTable bs =
    case runGetOrFail tableParser bs of
        Left (_, _, msg) ->
            Left msg

        -- Even if we parsed a table, leftover bytes indicate something
        -- could be wrong.
        Right (remainingBytes, _, _) | not (BSL.null remainingBytes) ->
            Left $ "Error: " <> show (BSL.length remainingBytes) <>
                   " byte(s) left unconsumed"

        Right (_, _, table) ->
            Right table

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

tableParser :: Get UnicodeWidthTable
tableParser = do
    magic <- getWord32le

    when (magic /= widthTableMagic) $
        fail "Table magic number invalid"

    version <- getWord8

    case version of
        1 -> tableV1Parser
        _ -> fail "Table version invalid"

tableV1Parser :: Get UnicodeWidthTable
tableV1Parser = do
    numRanges <- getWord32le

    let parseRange = do
            start <- getWord32le
            size <- getWord32le
            cols <- getWord8
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
