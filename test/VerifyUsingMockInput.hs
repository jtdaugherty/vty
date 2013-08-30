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

tests :: IO [Test]
tests = return []
