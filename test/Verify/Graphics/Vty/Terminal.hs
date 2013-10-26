module Verify.Graphics.Vty.Terminal where

import Graphics.Vty.Terminal.Mock

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

import Test.QuickCheck.Property

-- A list of terminals that should be supported.
-- This started with a list of terminals ubuntu supported. Then those terminals that really could
-- not be supported were removed. Then a few more were pruned until a reasonable looking set was
-- made.
terminals_of_interest :: [String]
terminals_of_interest = 
    [ "vt100"
    , "vt220"
    , "vt102"
    , "xterm-r5"
    , "xterm-xfree86"
    , "xterm-r6"
    , "xterm-256color"
    , "xterm-vt220"
    , "xterm-debian"
    , "xterm-mono"
    , "xterm-color"
    , "xterm"
    , "mach"
    , "mach-bold"
    , "mach-color"
    , "linux"
    , "ansi"
    , "hurd"
    , "Eterm"
    , "pcansi"
    , "screen-256color"
    , "screen-bce"
    , "screen-s"
    , "screen-w"
    , "screen"
    , "screen-256color-bce"
    , "sun"
    , "rxvt"
    , "rxvt-unicode"
    , "rxvt-basic"
    , "cygwin"
    ]

compare_mock_output :: MockData -> String -> IO Result
compare_mock_output mock_data expected_str = do
    out_bytes <- readIORef mock_data >>= return . UTF8.toRep
    let expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected_str
    if out_bytes /=  expected_bytes
        then return $ failed { reason = "bytes\n" ++ show out_bytes
                                        ++ "\nare not the expected bytes\n"
                                        ++ show expected_bytes
                             }
        else return succeeded

