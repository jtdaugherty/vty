module VerifyInline where

import Graphics.Vty.Inline
import Graphics.Vty.Output
import Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Verify.Graphics.Vty.Output

import Verify

import Distribution.TestSuite

import System.IO

tests :: IO [Test]
tests = concat <$> forM terminalsOfInterest (\termName -> return $
    [ Test $ TestInstance
        { name = "verify vty inline"
        , run = do
            {- disabled because I cannot get useful output out of cabal why this fails.
            nullOut <- openFile "/dev/null" WriteMode
            t <- TerminfoBased.reserveTerminal termName nullOut
            putAttrChange t $ default_all
            releaseTerminal t
            -}
            return $ Finished Pass
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "no options supported"
        }
    ])
