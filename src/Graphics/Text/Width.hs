-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -D_XOPEN_SOURCE #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Text.Width
  ( wcwidth
  , wcswidth
  , wctwidth
  , wctlwidth
  , safeWcwidth
  , safeWcswidth
  , safeWctwidth
  , safeWctlwidth
  )
where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

foreign import ccall unsafe "vty_mk_wcwidth" wcwidth :: Char -> Int

wcswidth :: String -> Int
wcswidth = foldl' (\l c -> wcwidth c + l) 0

wctwidth :: T.Text -> Int
wctwidth = T.foldl' (\l c -> wcwidth c + l) 0

wctlwidth :: TL.Text -> Int
wctlwidth = TL.foldl' (\l c -> wcwidth c + l) 0

-- XXX: Characters with unknown widths occupy 1 column?
--
-- Not sure if this is actually correct. I presume there is a
-- replacement character that is output by the terminal instead of the
-- character and this replacement character is 1 column wide. If this is
-- not true for all terminals then a per-terminal replacement character
-- width needs to be implemented.

-- | Returns the display width of a character. Assumes all characters
-- with unknown widths are 0 width.
safeWcwidth :: Char -> Int
safeWcwidth = max 0 . wcwidth

-- | Returns the display width of a string. Assumes all characters with
-- unknown widths are 0 width.
safeWcswidth :: String -> Int
safeWcswidth = foldl' (\l c -> safeWcwidth c + l) 0

-- | Returns the display width of a text. Assumes all characters with
-- unknown widths are 0 width.
safeWctwidth :: T.Text -> Int
safeWctwidth = T.foldl' (\l c -> safeWcwidth c + l) 0

-- | Returns the display width of a lazy text. Assumes all characters
-- with unknown widths are 0 width.
safeWctlwidth :: TL.Text -> Int
safeWctlwidth = TL.foldl' (\l c -> safeWcwidth c + l) 0
