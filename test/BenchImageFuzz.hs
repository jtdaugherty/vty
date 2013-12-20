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

random_image :: IO Image
random_image = rand

random_picture = pic_for_image <$> random_image

bench_0 = do
    vty <- mkVty
    (w,h) <- display_bounds $ terminal vty
    let pictures = replicateM 3000 random_picture
        bench ps = do
            forM ps (update vty)
            shutdown vty
    return $ Bench pictures bench

