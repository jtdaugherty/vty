{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Verify ( module Verify
              , module Control.DeepSeq
              , module Test.QuickCheck
              , module Test.QuickCheck.Modifiers
              , succeeded
              , failed
              , result
              , monadicIO
              , liftIO
              , liftBool
              , Test(..)
              , Prop.Result(..)
              )
    where

import Distribution.TestSuite hiding ( Result(..) )
import qualified Distribution.TestSuite as TS

import Test.QuickCheck hiding ( Result(..) )
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property hiding ( Result(..) )
import qualified Test.QuickCheck.Property as Prop
import Test.QuickCheck.Monadic ( monadicIO ) 

import qualified Codec.Binary.UTF8.String as UTF8

import Control.DeepSeq
import Control.Monad.State.Strict

import Data.IORef
import Data.Word

import Numeric ( showHex )

import System.IO
import System.Random

verify :: Testable t => String -> t -> Test
verify test_name p = Test $ TestInstance
  { name = test_name
  , run = do
    qc_result <- quickCheckResult p
    case qc_result of
      QC.Success {..} -> return $ Finished TS.Pass
      _               -> return $ Finished $ TS.Fail "TODO(corey): add failure message"
  , tags = []
  , options = []
  , setOption = \_ _ -> Left "no options supported"
  }

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
        ++ [ toEnum 0x309B .. toEnum 0xA4CF ]

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

data Bench where
    Bench :: forall v . NFData v => IO v -> (v -> IO ()) -> Bench

