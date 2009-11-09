{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Terminfo.Eval ( bytes_for_range )

import qualified System.Console.Terminfo as Terminfo

import Verify.Data.Terminfo.Parse
import Verify

import Control.Monad ( mapM_ )

import Data.Array.Unboxed
import Data.Maybe ( fromJust )
import Data.Word

import Numeric

caps = 
    [ "cup"
    , "sc"
    , "rc"
    , "setf"
    , "setb"
    , "setaf"
    , "setab"
    , "op"
    , "cnorm"
    , "civis"
    , "smcup"
    , "rmcup"
    , "clear"
    , "hpa"
    , "vpa"
    , "sgr"
    , "sgr0"
    ]

from_capname ti name = fromJust $ Terminfo.getCapability ti (Terminfo.tiGetStr name)

main = do
    ti <- Terminfo.setupTermFromEnv 
    mapM_ ( print_cap ti) caps
    run_test $ do
        mapM_ ( \cap -> let cap_str = from_capname ti cap
                        in verify ( "parse cap " ++ show cap ++ " -> " ++ show cap_str )
                                  ( verify_parse_cap cap_str $ const (liftResult succeeded) ) 
              ) caps
        verify "parse_non_paramaterized_caps" non_paramaterized_caps
        verify "parse cap string with literal %" literal_percent_caps
        verify "parse cap string with %i op" inc_first_two_caps
        verify "parse cap string with %pN op" push_param_caps
        return ()
    return ()

verify_parse_cap cap_string on_parse = do
    case parse_cap_expression cap_string of
        Left error -> liftResult $ failed { reason = "parse error " ++ show error }
        Right e -> on_parse e

non_paramaterized_caps (NonParamCapString cap) = do
    verify_parse_cap cap $ \e -> 
        let expected_count :: Word8 = toEnum $ length cap
            expected_bytes = map (toEnum . fromEnum) cap
            out_bytes = bytes_for_range e (0, expected_count)
        in verify_bytes_equal out_bytes expected_bytes

literal_percent_caps (LiteralPercentCap cap_string expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
        in verify_bytes_equal out_bytes expected_bytes

inc_first_two_caps (IncFirstTwoCap cap_string expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
        in verify_bytes_equal out_bytes expected_bytes
    
push_param_caps (PushParamCap cap_string expected_param_count expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
            out_param_count = param_count e
        in verify_bytes_equal out_bytes expected_bytes
           .&. out_param_count == expected_param_count

dec_print_param_caps (DecPrintCap cap_string expected_param_count expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
            out_param_count = param_count e
        in verify_bytes_equal out_bytes expected_bytes
           .&. out_param_count == expected_param_count

print_cap ti cap_name = do
    putStrLn $ cap_name ++ ": " ++ show (from_capname ti cap_name)

print_expression ti cap_name = do
    putStrLn $ cap_name ++ ": " ++ show (parse_cap_expression $ from_capname ti cap_name)

