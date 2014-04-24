{-# LANGUAGE QuasiQuotes #-}
module Main where

import Graphics.Vty.Config
import Graphics.Vty.Input.Events

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
-- comments should be ignored.
map "\ESC[B" KUp []
askfjla dfasjdflk jasdlkfj asdfj -- lines failing parse should be ignored
map "\ESC[1;3B" KDown [MAlt]
|]

exampleConfigConfig :: Config
exampleConfigConfig = Config
    { specifiedEscPeriod = def
    , debugLog = def
    , inputOverrides = [("\ESC[B", EvKey KUp []), ("\ESC[1;3B", EvKey KDown [MAlt])]
    }

exampleConfigParses :: IO ()
exampleConfigParses = assertEqual "example config parses as expected"
                                  exampleConfigConfig
                                  (runParseConfig "exampleConfig" exampleConfig)

main :: IO ()
main = defaultMain
    [ testCase "example config parses" $ exampleConfigParses
    ]

