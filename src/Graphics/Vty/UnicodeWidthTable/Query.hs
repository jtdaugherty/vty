{-# LANGUAGE TupleSections #-}
module Graphics.Vty.UnicodeWidthTable.Query
  ( buildUnicodeWidthTable
  )
where

import Control.Monad (forM)
import Data.Char (generalCategory, GeneralCategory(..))
import Data.Maybe (catMaybes)
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

-- | Construct a unicode character width table by querying the terminal
-- connected to stdout. This works by emitting characters to stdout
-- and then querying the terminal to determine the resulting cursor
-- position in order to measure character widths. Consequently this will
-- generate a lot of output and may take a while, depending on your
-- system performance.
buildUnicodeWidthTable :: IO UnicodeWidthTable
buildUnicodeWidthTable = do
    -- NB: this only covers some of the possible Unicode range and is
    -- sure to become stale eventually. Granted, at the time of this
    -- writing, even the latest version of Unicode (13) totals 143,859
    -- characters and that only gets us to code point 0x231f3 or so. But
    -- it's only a matter of time before the bound in this loop is too
    -- low.
    pairs <- fmap catMaybes $ forM ['\0'..'\x2FFFF'] $ \i ->
        if shouldConsider i
        then (Just . (i,)) <$> charWidth i
        else return Nothing

    return UnicodeWidthTable { unicodeWidthTableRanges = reverse $ mkRanges pairs
                             }
