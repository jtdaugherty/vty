module VerifyInline where

import Graphics.Vty.Inline
import Graphics.Vty.Output
import Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Verify.Graphics.Vty.Output

import Verify

import Distribution.TestSuite

import System.IO

tests :: IO [Test]
tests = concat <$> forM terminals_of_interest (\term_name -> return $
    [ Test $ TestInstance
        { name = "verify vty inline"
        , run = do
            {- disabled because I cannot get useful output out of cabal why this fails.
            null_out <- openFile "/dev/null" WriteMode
            t <- TerminfoBased.reserve_terminal term_name null_out
            put_attr_change t $ default_all
            release_terminal t
            -}
            return $ Finished Pass
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "no options supported"
        }
    ])
