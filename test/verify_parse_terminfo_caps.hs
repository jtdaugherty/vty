{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding ( catch )

import Data.Terminfo.Eval ( bytes_for_range )

import qualified System.Console.Terminfo as Terminfo

import Verify.Data.Terminfo.Parse
import Verify

import Control.Exception ( try, SomeException(..) )
import Control.Monad ( mapM_, forM_ )

import Data.Array.Unboxed
import Data.Maybe ( fromJust )
import Data.Word

import Numeric

-- A list of terminals that ubuntu includes a terminfo cap file for. 
-- Assuming that is a good place to start.
terminals_of_interest = 
    [ "wsvt25"
    , "wsvt25m"
    , "vt52"
    , "vt100"
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
    , "cons25"
    , "dumb"
    ]

-- If a terminal defines one of the caps then it's expected to be parsable.
caps_of_interest = 
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
    run_test $ do
        forM_ terminals_of_interest $ \term_name -> do
            liftIO $ putStrLn $ "testing parsing of caps for terminal: " ++ term_name
            mti <- liftIO $ try $ Terminfo.setupTerm term_name
            case mti of
                Left (_e :: SomeException) 
                    -> return ()
                Right ti -> do
                    forM_ caps_of_interest $ \cap_name -> do
                        liftIO $ putStrLn $ "\tparsing cap: " ++ cap_name
                        case Terminfo.getCapability ti (Terminfo.tiGetStr cap_name) of
                            Just cap_def -> do
                                verify ( "\tparse cap " ++ cap_name ++ " -> " ++ show cap_def )
                                       ( verify_parse_cap cap_def $ const ( return succeeded ) ) 
                                return ()
                            Nothing      -> do
                                return ()
        -- The quickcheck tests
        verify "parse_non_paramaterized_caps" non_paramaterized_caps
        verify "parse cap string with literal %" literal_percent_caps
        verify "parse cap string with %i op" inc_first_two_caps
        verify "parse cap string with %pN op" push_param_caps
        return ()
    return ()

verify_parse_cap cap_string on_parse = liftIOResult $ do
    parse_result <- parse_cap_expression cap_string
    case parse_result of
        Left error -> return $ failed { reason = "parse error " ++ show error }
        Right e -> on_parse e

non_paramaterized_caps (NonParamCapString cap) = do
    verify_parse_cap cap $ \e -> 
        let expected_count :: Word8 = toEnum $ length cap
            expected_bytes = map (toEnum . fromEnum) cap
            out_bytes = bytes_for_range e 0 expected_count
        in return $ verify_bytes_equal out_bytes expected_bytes

literal_percent_caps (LiteralPercentCap cap_string expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
        in return $ verify_bytes_equal out_bytes expected_bytes

inc_first_two_caps (IncFirstTwoCap cap_string expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
        in return $ verify_bytes_equal out_bytes expected_bytes
    
push_param_caps (PushParamCap cap_string expected_param_count expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
            out_param_count = param_count e
        in return $ if out_param_count == expected_param_count
            then verify_bytes_equal out_bytes expected_bytes
            else failed { reason = "out param count /= expected param count" }

dec_print_param_caps (DecPrintCap cap_string expected_param_count expected_bytes) = do
    verify_parse_cap cap_string $ \e ->
        let expected_count :: Word8 = toEnum $ length expected_bytes
            out_bytes = collect_bytes e
            out_param_count = param_count e
        in return $ if out_param_count == expected_param_count
            then verify_bytes_equal out_bytes expected_bytes
            else failed { reason = "out param count /= expected param count" }

print_cap ti cap_name = do
    putStrLn $ cap_name ++ ": " ++ show (from_capname ti cap_name)

print_expression ti cap_name = do
    parse_result <- parse_cap_expression $ from_capname ti cap_name
    putStrLn $ cap_name ++ ": " ++ show parse_result

