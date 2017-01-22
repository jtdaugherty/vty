{- benchmarks composing images using the renderChar operation.
 - This is what Yi uses in Yi.UI.Vty.drawText. Ideally a sequence of renderChar images horizontally
 - composed should provide no worse performance than a fill render op.
 -}
module BenchRenderChar where

import Graphics.Vty
import Verify

import Control.Monad ( forM_ )

import Data.Default (def)

bench0 = do
    vty <- mkVty def
    (w,h) <- displayBounds $ outputIface vty
    let testChars = return $ take 500 $ cycle $ [ c | c <- ['a'..'z']]
        bench d = do
            forM_ d $ \testChar -> do
                let testImage = testImageUsingChar testChar w h
                    outPic = picForImage testImage
                update vty outPic
            shutdown vty
    return $ Bench testChars bench

testImageUsingChar c w h
    = vertCat $ replicate (fromIntegral h)
              $ horizCat $ map (char defAttr) (replicate (fromIntegral w) c)

bench1 = do
    vty <- mkVty def
    (w,h) <- displayBounds $ outputIface vty
    let testChars = return $ take 500 $ cycle $ [ c | c <- ['a'..'z']]
        bench d = do
            forM_ d $ \testChar -> do
                let testImage = charFill defAttr testChar w h
                    outPic = picForImage testImage
                update vty outPic
            shutdown vty
    return $ Bench testChars bench
