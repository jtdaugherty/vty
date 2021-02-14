{-# OPTIONS_HADDOCK hide #-}
-- Copyright 2009 Corey O'Connor
module Codec.Binary.UTF8.Debug where

import Codec.Binary.UTF8.String ( encode )

import Data.Word

import Numeric

-- | Converts an array of ISO-10646 characters (Char type) to an array
-- of Word8 bytes that is the corresponding UTF8 byte sequence
utf8FromIso :: [Int] -> [Word8]
utf8FromIso = encode . map toEnum

ppUtf8 :: [Int] -> IO ()
ppUtf8 = print . map (($ "") . showHex) . utf8FromIso

