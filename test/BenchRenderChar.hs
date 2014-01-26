{- benchmarks composing images using the renderChar operation.
 - This is what Yi uses in Yi.UI.Vty.drawText. Ideally a sequence of renderChar images horizontally
 - composed should provide no worse performance than a fill render op.
 -}
module BenchRenderChar where

import Graphics.Vty
import Verify

import Control.Monad ( forM_ )

import Data.Default (def)

bench_0 = do
    vty <- mkVty def
    (w,h) <- display_bounds $ output_iface vty
    let test_chars = return $ take 500 $ cycle $ [ c | c <- ['a'..'z']]
        bench d = do
            forM_ d $ \test_char -> do
                let test_image = test_image_using_char test_char w h
                    out_pic = pic_for_image test_image
                update vty out_pic
            shutdown vty
    return $ Bench test_chars bench

test_image_using_char c w h 
    = vert_cat $ replicate (fromIntegral h)
               $ horiz_cat $ map (char def_attr) (replicate (fromIntegral w) c)

bench_1 = do
    vty <- mkVty def
    (w,h) <- display_bounds $ output_iface vty
    let test_chars = return $ take 500 $ cycle $ [ c | c <- ['a'..'z']]
        bench d = do
            forM_ d $ \test_char -> do
                let test_image = char_fill def_attr test_char w h
                    out_pic = pic_for_image test_image
                update vty out_pic
            shutdown vty
    return $ Bench test_chars bench
