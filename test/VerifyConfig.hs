{-# LANGUAGE QuasiQuotes #-}
module Main where

import Graphics.Vty.Config

import Control.Applicative
import Control.Exception
import Control.Lens ((^.))
import Control.Monad

import Data.Default
import Data.String.QQ

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Printf

exampleConfig :: String
exampleConfig = [s|
map "\ESC[B" KUp []
map "\ESC[1;3B" KDown [MAlt]
|]

exampleConfigParses :: IO ()
exampleConfigParses = return ()

main :: IO ()
main = defaultMain
    [ testCase "example config parses" $ exampleConfigParses
    ]

