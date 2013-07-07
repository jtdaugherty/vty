module VerifyInline where

import Graphics.Vty.Inline
import Graphics.Vty.Terminal

import Verify

import Distribution.TestSuite

tests :: IO [Test]
tests = return
    [ Test $ TestInstance
        { name = "verify vty inline"
        , run = do
            t <- current_terminal
            put_attr_change t $ default_all
            return $ Finished Pass
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "no options supported"
        }
    ]

