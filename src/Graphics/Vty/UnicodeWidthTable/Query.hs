{-# LANGUAGE TupleSections #-}
module Graphics.Vty.UnicodeWidthTable.Query
  ( buildUnicodeWidthTable
  )
where

import Control.Monad (forM)
import Data.Char (generalCategory, GeneralCategory(..))
import System.Console.ANSI (getCursorPosition)
import Text.Printf (printf)

import Graphics.Vty.UnicodeWidthTable.Types

shouldConsider :: Char -> Bool
shouldConsider c =
    case generalCategory c of
        Control     -> False
        NotAssigned -> False
        Surrogate   -> False
        _           -> True

charWidth :: Char -> IO Int
charWidth c = do
    printf "\r"
    putChar c
    Just (_, col) <- getCursorPosition
    return col

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
--
-- This only covers some of the possible Unicode range and is sure to
-- become stale eventually. Granted, at the time of this writing, even
-- the latest version of Unicode (13) totals 143,859 characters and that
-- only gets us to code point 0x231f3 or so. But it's only a matter of
-- time before this bound is too low.
unicodeTableUpperBound :: Char
unicodeTableUpperBound = '\x2FFFF'

-- | Construct a unicode character width table by querying the terminal
-- connected to stdout. This works by emitting characters to stdout
-- and then querying the terminal to determine the resulting cursor
-- position in order to measure character widths. Consequently this will
-- generate a lot of output and may take a while, depending on your
-- system performance.
buildUnicodeWidthTable :: IO UnicodeWidthTable
buildUnicodeWidthTable = do
    pairs <- forM (filter shouldConsider ['\0'..unicodeTableUpperBound]) $ \i ->
        (i,) <$> charWidth i

    return UnicodeWidthTable { unicodeWidthTableRanges = reverse $ mkRanges pairs
                             }
