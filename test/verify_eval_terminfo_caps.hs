{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Data.Marshalling

import Data.Terminfo.Eval 
import Data.Terminfo.Parse

import qualified System.Console.Terminfo as Terminfo

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
                        liftIO $ putStrLn $ "\tevaluating cap: " ++ cap_name
                        case Terminfo.getCapability ti (Terminfo.tiGetStr cap_name) of
                            Just cap_def -> do
                                verify ( "\teval cap " ++ cap_name ++ " -> " ++ show cap_def )
                                       ( verify_eval_cap cap_def )
                                return ()
                            Nothing      -> do
                                return ()
        return ()
    return ()

verify_eval_cap cap_string
    = case parse_cap_expression cap_string of
        Left error -> 
            liftResult $ failed { reason = "parse error " ++ show error }
        Right expr -> 
            forAll (vector 9) $ \input_values -> 
                let byte_count = cap_expression_required_bytes expr input_values
                in liftIOResult $ do
                    start_ptr :: Ptr Word8 <- mallocBytes (fromEnum byte_count)
                    end_ptr <- serialize_cap_expression expr input_values start_ptr
                    free start_ptr
                    case end_ptr `minusPtr` start_ptr of
                        count | count < 0        -> 
                                    return $ failed { reason = "End pointer before start pointer." }
                              | toEnum count > byte_count -> 
                                    return $ failed { reason = "End pointer past end of buffer by " 
                                                             ++ show (toEnum count - byte_count) 
                                                    }
                              | otherwise        -> 
                                    return succeeded

