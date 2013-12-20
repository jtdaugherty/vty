{-# LANGUAGE BangPatterns #-}
module BenchNoDiffOpt where

{-# LANGUAGE BangPatterns #-}
import Graphics.Vty
import Verify

import Control.Concurrent( threadDelay )
import Control.Monad( liftM2 )

import qualified Data.ByteString.Char8 as B
import Data.List

import System.Environment( getArgs )
import System.IO
import System.Random

bench_0 = do 
    let fixed_gen = mkStdGen 0
    setStdGen fixed_gen
    vty <- mkVty
    (w,h) <- display_bounds $ terminal vty
    let images = return $ (image_0, image_1)
        image_0 = char_fill def_attr 'X' w h
        image_1 = char_fill def_attr '0' w h
        bench d = do
            flip_out vty 300 image_0 image_1
            shutdown vty
    return $ Bench images bench

flip_out vty n image_0 image_1 = 
    let !p_left  = pic_for_image image_0
        !p_right = pic_for_image image_1
        w_left  0 = return ()
        w_left  n = update vty p_left  >> w_right (n-1)
        w_right 0 = return ()
        w_right n = update vty p_right >> w_left  (n-1)
    in w_left n
