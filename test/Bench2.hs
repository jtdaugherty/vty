{-# LANGUAGE BangPatterns #-}
import Graphics.Vty
import System.IO
import System.Environment( getArgs )
import Control.Concurrent( threadDelay )
import System.Random
import Data.List
import Control.Monad( liftM2 )

import qualified Data.ByteString.Char8 as B

main = do 
    let fixed_gen = mkStdGen 0
    setStdGen fixed_gen
    args <- getArgs
    vt <- mkVty
    (w, h) <- getSize vt
    let !image_0 = renderFill attr 'X' w h
    let !image_1 = renderFill attr '0' w h
    run vt image_0 image_1
    shutdown vt

run vt image_0 image_1 = run' vt 300 image_0 image_1

run' vt 0 image_0 image_1 = return ()
run' vt n image_0 image_1 = do
    let p = pic { pImage = image_0 }
    update vt p
    run' vt (n - 1) image_1 image_0

