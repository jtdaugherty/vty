module BenchImageFuzz where
import Graphics.Vty
import Graphics.Vty.Debug

import Verify.Graphics.Vty.Image
import Verify

import Control.Applicative
import Control.Monad

import System.Random

rand :: Arbitrary a => IO a
rand = head <$> sample' arbitrary

randomImage :: IO Image
randomImage = rand

randomPicture = picForImage <$> randomImage

bench0 = do
    vty <- mkVty defaultConfig
    (w,h) <- displayBounds $ outputIface vty
    let pictures = replicateM 3000 randomPicture
        bench ps = do
            forM ps (update vty)
            shutdown vty
    return $ Bench pictures bench

