{-# LANGUAGE QuasiQuotes #-}
module Main where

import Graphics.Vty.Config
import Graphics.Vty.Input.Events

import Data.String.QQ
import qualified Data.ByteString.Char8 as B

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

exampleConfig :: B.ByteString
exampleConfig = B.pack [s|
-- comments should be ignored.
map _ "\ESC[B" KUp []
askfjla dfasjdflk jasdlkfj asdfj -- lines failing parse should be ignored
map _ "\ESC[1;3B" KDown [MAlt]
map "xterm" "\ESC[1;3B" KDown [MAlt]
map "xterm-256-color" "\ESC[1;3B" KDown [MAlt]
debugLog "/tmp/vty-debug.txt"
|]

exampleConfigConfig :: VtyUserConfig
exampleConfigConfig = defaultConfig
    { configDebugLog = Just "/tmp/vty-debug.txt"
    , configInputMap = [ (Nothing, "\ESC[B", EvKey KUp [])
                       , (Nothing, "\ESC[1;3B", EvKey KDown [MAlt])
                       , (Just "xterm", "\ESC[1;3B", EvKey KDown [MAlt])
                       , (Just "xterm-256-color", "\ESC[1;3B", EvKey KDown [MAlt])
                       ]
    }

exampleConfigParses :: IO ()
exampleConfigParses =
  assertEqual "example config parses as expected"
    exampleConfigConfig
    (runParseConfig "exampleConfig" exampleConfig)

main :: IO ()
main = defaultMain
    [ testCase "example config parses" $ exampleConfigParses
    ]
