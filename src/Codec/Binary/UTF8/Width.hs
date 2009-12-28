-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -D_XOPEN_SOURCE -fno-cse #-}
{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# INCLUDE <wchar.h> #-}
module Codec.Binary.UTF8.Width ( wcwidth
                               , wcswidth
                               )
    where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr

-- import Numeric ( showHex )

import System.IO.Unsafe

wcwidth :: Char -> Int
wcwidth c = unsafePerformIO (withCWString [c] $! \ws -> do
    wc <- peek ws
    -- putStr $ "wcwidth(0x" ++ showHex (fromEnum wc) "" ++ ")"
    let !w = fromIntegral $! wcwidth' wc
    -- putStrLn $ " -> " ++ show w
    return w
    )
{-# NOINLINE wcwidth #-}

foreign import ccall unsafe "mk_wcwidth" wcwidth' :: CWchar -> CInt

wcswidth :: String -> Int
wcswidth str = unsafePerformIO (withCWStringLen str $! \(ws, ws_len) -> do
    -- putStr $ "wcswidth(...)"
    let !w = fromIntegral $! wcswidth' ws (fromIntegral ws_len)
    -- putStrLn $ " -> " ++ show w
    return w
    )
{-# NOINLINE wcswidth #-}

foreign import ccall unsafe "mk_wcswidth" wcswidth' :: Ptr CWchar -> CSize -> CInt
