-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -D_XOPEN_SOURCE #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Text.Width ( wcwidth
                           , wcswidth
                           , safe_wcwidth
                           , safe_wcswidth
                           )
    where

foreign import ccall unsafe "vty_mk_wcwidth" wcwidth :: Char -> Int

wcswidth :: String -> Int
wcswidth = sum . map wcwidth

-- XXX: Characters with unknown widths occupy 1 column?
-- 
-- Not sure if this is actually correct.  I presume there is a replacement character that is output
-- by the terminal instead of the character and this replacement character is 1 column wide. If this
-- is not true for all terminals then a per-terminal replacement character width needs to be
-- implemented.

-- | Returns the display width of a character. Assumes all characters with unknown widths are 0 width
safe_wcwidth :: Char -> Int
safe_wcwidth c = case wcwidth c of
    i   | i < 0 -> 0
        | otherwise -> i

-- | Returns the display width of a string. Assumes all characters with unknown widths are 0 width
safe_wcswidth :: String -> Int
safe_wcswidth str = case wcswidth str of
    i   | i < 0 -> 0
        | otherwise -> i

