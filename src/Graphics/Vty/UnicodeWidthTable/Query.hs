{-# LANGUAGE TupleSections #-}
module Graphics.Vty.UnicodeWidthTable.Query
  ( buildUnicodeWidthTable
  , defaultUnicodeTableUpperBound
  )
where

import Control.Monad (forM)
import Data.Char (generalCategory, GeneralCategory(..))

import Graphics.Vty.UnicodeWidthTable.Types

shouldConsider :: Char -> Bool
shouldConsider c =
    case generalCategory c of
        Control     -> False
        NotAssigned -> False
        Surrogate   -> False
        _           -> True

-- | Convert a sequence of character/width pairs into a list of
-- run-length encoded ranges. This function assumes the pairs come
-- sorted by character ordinal value. It does not require that the
-- character range is fully covered by the sequence.
--
-- The result of this function is a list of ranges in reverse order
-- relative to the input sequence.
mkRanges :: [(Char, Int)] -> [WidthTableRange]
mkRanges pairs =
    let convertedPairs = convert <$> pairs
        convert (c, i) = (fromIntegral $ fromEnum c, fromIntegral i)

        go Nothing finishedRanges [] = finishedRanges
        go (Just r) finishedRanges [] = r:finishedRanges

        go Nothing finishedRanges ((c, width):rest) =
            go (Just $ WidthTableRange c 1 width) finishedRanges rest

        go (Just r@(WidthTableRange prevCh sz prevWidth)) finishedRanges ((c, width):rest) =
            if c == prevCh + sz && prevWidth == width
            then go (Just (WidthTableRange prevCh (sz + 1) prevWidth)) finishedRanges rest
            else go (Just (WidthTableRange c 1 width)) (r:finishedRanges) rest

    in go Nothing [] convertedPairs

-- The uppermost code point to consider when building Unicode width
-- tables.
defaultUnicodeTableUpperBound :: Char
defaultUnicodeTableUpperBound = '\xe0000'

-- | Construct a unicode character width table. This works by using the
-- provided function to obtain the appropriate width for each character
-- in a wide range of Unicode code points, which on some platforms
-- may perform local terminal operations or may interact with system
-- libraries. Depending on how the provided width function works, this
-- may need to be run only in a terminal that is not actively controlled
-- by a Vty handle.
--
-- The character argument specifies the upper bound code point to test
-- when building the table. This allows callers to decide how much of
-- the Unicode code point space to scan when building the table.
--
-- This does not handle exceptions.
buildUnicodeWidthTable :: (Char -> IO Int) -> Char -> IO UnicodeWidthTable
buildUnicodeWidthTable charWidth tableUpperBound = do
    pairs <- forM (filter shouldConsider ['\0'..tableUpperBound]) $ \i ->
        (i,) <$> charWidth i

    return UnicodeWidthTable { unicodeWidthTableRanges = reverse $ mkRanges pairs
                             }
