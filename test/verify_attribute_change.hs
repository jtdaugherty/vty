module Main where

import Graphics.Vty.AttributeChange
import Graphics.Vty.Terminal

import Verify

main = run_test $ do
    t <- terminal_handle
    put_attr_change t $ default_all
    return ()
