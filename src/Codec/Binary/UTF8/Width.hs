-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -D_XOPEN_SOURCE -fno-cse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE <wchar.h> #-}
module Codec.Binary.UTF8.Width ( wcwidth
                               , wcswidth
                               )
    where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable

import Numeric ( showHex )

import System.IO.Unsafe

wcwidth :: Char -> Int
wcwidth c = unsafePerformIO (withCWString [c] $ \ws -> do
    wc <- peek ws
    putStr $ "wcwidth(0x" ++ showHex (fromEnum wc) "" ++ ")"
    w <- wcwidth' wc  >>= return . fromIntegral
    putStrLn $ " -> " ++ show w
    return w
    )
{-# NOINLINE wcwidth #-}

foreign import ccall "mk_wcwidth" wcwidth' :: CWchar -> IO CInt

wcswidth :: String -> Int
wcswidth str = sum $ map wcwidth str

