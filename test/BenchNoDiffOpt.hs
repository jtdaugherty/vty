{-# LANGUAGE BangPatterns #-}
module BenchNoDiffOpt where

{-# LANGUAGE BangPatterns #-}
import Graphics.Vty
import Verify

import Control.Concurrent( threadDelay )
import Control.Monad( liftM2 )

import qualified Data.ByteString.Char8 as B
import Data.Default (def)
import Data.List

import System.Environment( getArgs )
import System.IO
import System.Random

bench0 = do
    let fixedGen = mkStdGen 0
    setStdGen fixedGen
    vty <- mkVty def
    (w,h) <- displayBounds $ outputIface vty
    let images = return $ (image0, image1)
        image0 = charFill defAttr 'X' w h
        image1 = charFill defAttr '0' w h
        bench d = do
            flipOut vty 300 image0 image1
            shutdown vty
    return $ Bench images bench

flipOut vty n image0 image1 =
    let !pLeft  = picForImage image0
        !pRight = picForImage image1
        wLeft  0 = return ()
        wLeft  n = update vty pLeft  >> wRight (n-1)
        wRight 0 = return ()
        wRight n = update vty pRight >> wLeft  (n-1)
    in wLeft n
