{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- TODO: Cannot conditionally compile tests?
-- Seems reasonable that this should fail, with the expectation of failure
-- on windows systems.
module VerifyEvalTerminfoCaps where

import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString)
import Data.Terminfo.Eval
import Data.Terminfo.Parse
import Control.DeepSeq

#ifdef TERMINFO
import qualified System.Console.Terminfo as Terminfo
#endif

import Verify
import Verify.Graphics.Vty.Output

import Control.Applicative ( (<$>) )
import Control.Exception ( try, SomeException(..) )

import Control.Monad ( mapM_, forM, forM_ )

import Data.Maybe ( fromJust )
import Data.Word

import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, minusPtr)
import Numeric

-- If a terminal defines one of the caps then it's expected to be parsable.
capsOfInterest =
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

tests :: IO [Test]
#ifndef TERMINFO
tests = return mempty
#else
tests = do
    fmap concat $ forM terminalsOfInterest $ \termName -> do
        putStrLn $ "adding tests for terminal: " ++ termName
        mti <- try $ Terminfo.setupTerm termName
        case mti of
            Left (_e :: SomeException)
                -> return []
            Right ti -> do
                fmap concat $ forM capsOfInterest $ \capName -> do
                    case Terminfo.getCapability ti (Terminfo.tiGetStr capName) of
                        Just capDef -> do
                            putStrLn $ "\tadding test for cap: " ++ capName
                            let testName = termName ++ "(" ++ capName ++ ")"
                            case parseCapExpression capDef of
                                Left error -> return [verify testName (failed {reason = "parse error " ++ show error})]
                                Right !cap_expr -> return [verify testName (verifyEvalCap cap_expr)]
                        Nothing      -> do
                            return []

fromCapname ti name = fromJust $ Terminfo.getCapability ti (Terminfo.tiGetStr name)

{-# NOINLINE verifyEvalCap #-}
verifyEvalCap :: CapExpression -> Int -> Property
verifyEvalCap expr !junkInt = do
    forAll (vector 9) $ \inputValues ->
        let builder = writeCapExpr expr inputValues
            !byteCount = LBS.length $ toLazyByteString builder
        in liftIOResult $
           if byteCount > 0 then
               return succeeded
           else
               return $ failed { reason = "output byte count is " ++ show byteCount }
#endif
