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

buildUnicodeWidthTable :: IO UnicodeWidthTable
buildUnicodeWidthTable = do
    pairs <- fmap catMaybes $ forM ['\0'..'\x2FFFF'] $ \i ->
        if shouldConsider i
        then (Just . (i,)) <$> charWidth i
        else return Nothing

    return UnicodeWidthTable { unicodeWidthTableRanges = reverse $ mkRanges pairs
                             }
