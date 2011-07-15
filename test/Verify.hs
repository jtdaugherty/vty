{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Verify ( module Verify
              , module Test.QuickCheck
              , succeeded
              , failed
              , result
              , Result(..)
              , monadicIO
              , liftIO
              , liftBool
              )
    where

import Test.QuickCheck hiding ( Result(..) )
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Property 
import Test.QuickCheck.Monadic ( monadicIO ) 

import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad.State.Strict

import Data.IORef
import Data.Word

import Numeric ( showHex )

import System.IO
import System.Random

type Test = StateT TestState IO

data TestState = TestState
    { results_ref :: IORef [QC.Result]
    }

run_test :: Test () -> IO ()
run_test t = do
    s <- newIORef [] >>= return . TestState
    s' <- runStateT t s >>= return . snd
    results <- readIORef $ results_ref s'
    let fail_results = [ fail_result | fail_result@(QC.Failure {}) <- results ]
    case fail_results of
        [] -> putStrLn "state: PASS"
        rs  -> do
            putStrLn "state: FAIL"
            putStrLn $ "fail_count: " ++ show (length rs)

verify :: Testable prop => String -> prop -> Test QC.Result
verify prop_name prop = do
    liftIO $ putStrLn $ "verify " ++ prop_name
    get >>= \s -> do
        r <- liftIO $ quickCheckResult prop 
        liftIO $ modifyIORef (results_ref s) (\rs -> r : rs)
        return r

data SingleColumnChar = SingleColumnChar Char
    deriving (Show, Eq)

instance Arbitrary SingleColumnChar where
    arbitrary = elements $ map SingleColumnChar [toEnum 0x21 .. toEnum 0x7E]

data DoubleColumnChar = DoubleColumnChar Char
    deriving (Eq)

instance Show DoubleColumnChar where
    show (DoubleColumnChar c) = "(0x" ++ showHex (fromEnum c) "" ++ ") ->" ++ UTF8.encodeString [c]

instance Arbitrary DoubleColumnChar where
    arbitrary = elements $ map DoubleColumnChar $ 
           [ toEnum 0x3040 .. toEnum 0x3098 ] 
        ++ [ toEnum 0x309B .. toEnum 0xA4CF]


liftIOResult :: Testable prop => IO prop -> Property
liftIOResult = morallyDubiousIOProperty

#if __GLASGOW_HASKELL__ <= 701
instance Random Word where
    random g = 
        let (i :: Int, g') = random g
        in (toEnum i, g')
    randomR (l,h) g =
        let (i :: Int, g') = randomR (fromEnum l,fromEnum h) g
        in (toEnum i, g')
#endif

