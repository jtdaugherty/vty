{-# LANGUAGE ScopedTypeVariables #-}
{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyUsingMockInput where
import Verify

import Graphics.Vty

import Control.Monad

import System.Posix.Env
import System.IO

data InputSpec
    = KeySeq String -- | input sequence encoded as a string. Regardless, the input is read a byte at a time.
    | Delay Int -- | millisecond delay

type ExpectedSpec = [Event]

tests :: IO [Test]
tests = return []

