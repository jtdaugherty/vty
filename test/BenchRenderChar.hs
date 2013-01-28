{- benchmarks composing images using the renderChar operation.
 - This is what Yi uses in Yi.UI.Vty.drawText. Ideally a sequence of renderChar images horizontally
 - composed should provide no worse performance than a fill render op.
 -}
module BenchRenderChar where

import Graphics.Vty

import Control.Monad ( forM_ )

bench_0 = do
    vty <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vty
    let test_chars = take 500 $ cycle $ [ c | c <- ['a'..'z']]
    forM_ test_chars $ \test_char -> do
        let test_image = test_image_using_char test_char w h
            out_pic = pic_for_image test_image
        update vty out_pic
    shutdown vty

test_image_using_char c w h 
    = vert_cat $ replicate (fromIntegral h)
               $ horiz_cat $ map (char def_attr) (replicate (fromIntegral w) c)

bench_1 = do
    vty <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vty
    let test_chars = take 500 $ cycle $ [ c | c <- ['a'..'z']]
    forM_ test_chars $ \test_char -> do
        let test_image = char_fill def_attr test_char w h
            out_pic = pic_for_image test_image
        update vty out_pic
    shutdown vty
