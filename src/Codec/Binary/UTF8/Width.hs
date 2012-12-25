-- Copyright 2009 Corey O'Connor
{-# OPTIONS_GHC -D_XOPEN_SOURCE -fno-cse #-}
{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Codec.Binary.UTF8.Width ( wcwidth
                               , wcswidth
                               )
    where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr

import System.IO.Unsafe

wcwidth :: Char -> Int
wcwidth c = unsafePerformIO (withCWString [c] $! \ws -> do
    wc <- peek ws
    let !w = fromIntegral $! wcwidth' wc
    return w
    )
{-# NOINLINE wcwidth #-}

foreign import ccall unsafe "vty_mk_wcwidth" wcwidth' :: CWchar -> CInt

wcswidth :: String -> Int
wcswidth str = unsafePerformIO (withCWStringLen str $! \(ws, ws_len) -> do
    let !w = fromIntegral $! wcswidth' ws (fromIntegral ws_len)
    return w
    )
{-# NOINLINE wcswidth #-}

foreign import ccall unsafe "vty_mk_wcswidth" wcswidth' :: Ptr CWchar -> CSize -> CInt
