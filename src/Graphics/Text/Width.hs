-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -D_XOPEN_SOURCE #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Text.Width ( wcwidth
                           , wcswidth
                           , safeWcwidth
                           , safeWcswidth
                           )
    where

import Foreign.C.Types (CInt(..))

foreign import ccall unsafe "vty_mk_wcwidth" c_wcwidth :: Char -> CInt

wcwidth :: Char -> Int
wcwidth = fromIntegral . c_wcwidth

wcswidth :: String -> Int
wcswidth = sum . map wcwidth

-- XXX: Characters with unknown widths occupy 1 column?
-- 
-- Not sure if this is actually correct.  I presume there is a replacement character that is output
-- by the terminal instead of the character and this replacement character is 1 column wide. If this
-- is not true for all terminals then a per-terminal replacement character width needs to be
-- implemented.

-- | Returns the display width of a character. Assumes all characters with unknown widths are 0 width
safeWcwidth :: Char -> Int
safeWcwidth = max 0 . wcwidth

-- | Returns the display width of a string. Assumes all characters with unknown widths are 0 width
safeWcswidth :: String -> Int
safeWcswidth = sum . map safeWcwidth
