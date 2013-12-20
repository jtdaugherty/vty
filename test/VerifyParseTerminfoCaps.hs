{-# LANGUAGE NamedFieldPuns #-}
module VerifyParseTerminfoCaps where

import Prelude hiding ( catch )

import qualified System.Console.Terminfo as Terminfo

import Verify.Data.Terminfo.Parse
import Verify.Graphics.Vty.Output
import Verify

import Data.Maybe ( catMaybes, fromJust )
import Data.Word

import Numeric

-- If a terminal defines one of the caps then it's expected to be parsable.
-- TODO: reduce duplication with terminfo terminal implementation.
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

tests :: IO [Test]
tests = do
    parse_tests <- concat <$> forM terminals_of_interest ( \term_name -> do
        mti <- liftIO $ try $ Terminfo.setupTerm term_name
        case mti of
            Left (_e :: SomeException)
                -> return []
            Right ti -> do
                concat <$> forM caps_of_interest ( \cap_name -> do
                    liftIO $ putStrLn $ "\tparsing cap: " ++ cap_name
                    case Terminfo.getCapability ti (Terminfo.tiGetStr cap_name) of
                        Just cap_def -> do
                            return [ verify ( "\tparse cap " ++ cap_name ++ " -> " ++ show cap_def )
                                            ( verify_parse_cap cap_def $ const (return succeeded)  ) ]
                        Nothing      -> do
                            return []
                    )
        )
    -- The quickcheck tests
    return $ [ verify "parse_non_paramaterized_caps" non_paramaterized_caps
             , verify "parse cap string with literal %" literal_percent_caps
             , verify "parse cap string with %i op" inc_first_two_caps
             , verify "parse cap string with %pN op" push_param_caps
             ] ++ parse_tests

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

