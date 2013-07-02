{-# LANGUAGE NoMonomorphismRestriction #-}
-- Copyright 2009 Corey O'Connor
module Codec.Binary.UTF8.Debug 
    where

import Codec.Binary.UTF8.String ( encode )

import Data.Word

import Numeric

-- | Converts an array of ISO-10646 characters (Char type) to an array of Word8 bytes that is the
-- corresponding UTF8 byte sequence
utf8_from_iso :: [Int] -> [Word8]
utf8_from_iso = encode . map toEnum

pp_utf8 :: [Int] -> IO ()
pp_utf8 = print . map (\f -> f "") . map showHex . utf8_from_iso

