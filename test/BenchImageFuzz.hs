{-# LANGUAGE MultiWayIf #-}
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

random_image w h = do
    SingleAttrSingleSpanStack i _ _ _ <- rand
    return i

random_picture w h = pic_for_image <$> random_image w h

bench_0 = do
    vty <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vty
    let pictures = replicateM 100 (random_picture w h)
        bench ps = do
            forM ps (update vty)
            shutdown vty
    return $ Bench pictures bench

