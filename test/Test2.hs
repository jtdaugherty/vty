module Main where
import Graphics.Vty

main = do
    vty <- mkVty
    (sx,sy) <- getSize vty
    update vty (pic { pImage = renderFill (setBG red attr) 'X' sx sy })
    refresh vty
    getEvent vty
    shutdown vty
    putStrLn "Done!"
    return ()

