{- benchmarks composing images using the renderChar operation.
 - This is what Yi uses in Yi.UI.Vty.drawText. Ideally a sequence of renderChar images horizontally
 - composed should provide no worse performance than a fill render op.
 -}
import Graphics.Vty

import Control.Monad ( forM_ )

import System.Time

main = do
    vty <- mkVty
    (w, h) <- getSize vty
    let test_chars = take 500 $ cycle $ [ c | c <- ['a'..'z']]
    start_time_0 <- getClockTime
    forM_ test_chars $ \test_char -> do
        let test_image = test_image_using_renderChar test_char w h
            out_pic = pic { pImage = test_image }
        update vty out_pic
    end_time_0 <- getClockTime
    let start_time_1 = end_time_0
    forM_ test_chars $ \test_char -> do
        let test_image = renderFill attr test_char w h
            out_pic = pic { pImage = test_image }
        update vty out_pic
    end_time_1 <- getClockTime
    shutdown vty
    putStrLn $ timeDiffToString $ diffClockTimes end_time_0 start_time_0
    putStrLn $ timeDiffToString $ diffClockTimes end_time_1 start_time_1

test_image_using_renderChar c w h = vertcat $ replicate h $ horzcat $ map (renderChar attr) (replicate w c)

