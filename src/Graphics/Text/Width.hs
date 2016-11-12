-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -D_XOPEN_SOURCE #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Text.Width ( wcwidth
                           , wcswidth
                           , safeWcwidth
                           , safeWcswidth
                           )
    where

foreign import ccall unsafe "vty_mk_wcwidth" wcwidth :: Char -> Int

wcswidth :: String -> Int
wcswidth = sum . map wcwidth

-- | Returns the display width of a character. Assumes all characters
-- with unknown widths are 0 width. This is equivalent to wcwidth and is
-- provided for backwards compatibility.
safeWcwidth :: Char -> Int
safeWcwidth = max 0 . wcwidth

-- | Returns the display width of a string. Assumes all characters with
-- unknown widths are 0 width. This is equivalent to wcwidth and is
-- provided for backwards compatibility.
safeWcswidth :: String -> Int
safeWcswidth = sum . map safeWcwidth
