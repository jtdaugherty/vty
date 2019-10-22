{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Verify
  ( module Verify
  , module Control.Applicative
  , module Control.DeepSeq
  , module Control.Exception
  , module Control.Monad
  , module Test.QuickCheck
  , module Test.QuickCheck.Modifiers
  , module Text.Printf
  , succeeded
  , failed
  , monadicIO
  , liftIO
  , liftBool
  , Test(..)
  , Prop.Result(..)
  )
where

import Control.Exception ( bracket, try, SomeException(..) )

import Distribution.TestSuite hiding ( Result(..) )
import qualified Distribution.TestSuite as TS

import Test.QuickCheck hiding ( Result(..) )
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property hiding ( Result(..) )
import qualified Test.QuickCheck.Property as Prop
import Test.QuickCheck.Monadic ( monadicIO )

import Text.Printf
import Data.Char ( generalCategory, GeneralCategory(NotAssigned) )

import qualified Codec.Binary.UTF8.String as UTF8

import Control.Applicative hiding ( (<|>) )
import Control.DeepSeq
import Control.Monad ( forM, mapM, mapM_, forM_ )
import Control.Monad.State.Strict

import Numeric ( showHex )

verify :: Testable t => String -> t -> Test
verify testName p = Test $ TestInstance
  { name = testName
  , run = do
    qcResult <- quickCheckWithResult (stdArgs {chatty = False}) p
    case qcResult of
        QC.Success {..} -> return $ Finished TS.Pass
        QC.Failure {numShrinks,reason} -> return $ Finished
            $ TS.Fail $ "After "
                      ++ show numShrinks ++ " shrinks determined failure to be: "
                      ++ show reason
        _ -> return $ Finished $ TS.Fail "TODO(corey): add failure message"
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
        filter (\c -> generalCategory c /= NotAssigned) $
           -- See https://www.unicode.org/Public/12.0.0/ucd/EastAsianWidth.txt
           -- Start from: CJK RADICAL REPEAT
              [ toEnum 0x2E80 .. toEnum 0x303E ]
           -- Skip narrow: 303F IDEOGRAPHIC HALF FILL SPACE
           ++ [ toEnum 0x3040 .. toEnum 0x3247 ]
           -- Avoid ambiguous: 3248..324F CIRCLED NUMBER TEN ON BLACK SQUARE
           ++ [ toEnum 0x3250 .. toEnum 0x4DBF ]
           -- Skip narrow: 4DC0..4DFF HEXAGRAM FOR THE CREATIVE HEAVEN
           ++ [ toEnum 0x4E00 .. toEnum 0xA4C6 ]
           -- End at: YI RADICAL KE

liftIOResult :: Testable prop => IO prop -> Property
liftIOResult = ioProperty

data Bench where
    Bench :: forall v . NFData v => IO v -> (v -> IO ()) -> Bench
