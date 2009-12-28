module Main
    where

import Graphics.Vty
import Graphics.Vty.Debug

import System.IO

main :: IO ()
main = do
    vty <- mkVty
    (sx, sy) <- getSize vty
    play vty sx sy

play :: Vty -> Int -> Int -> IO ()
play vty sx sy = 
    let 
        testScreen = pic {
            pCursor = NoCursor
          , pImage = box 10 10 }
    in do
      update vty testScreen
      getEvent vty
      shutdown vty
      return ()

box :: Int -> Int -> Image
box w h =
    let
        corner = renderChar attr '+'
        vertLine = renderFill attr '|' 1 (h - 2)
        horizLine = corner <|> renderHFill attr '-' (w - 2) <|> corner
        centerArea = vertLine <|> renderFill attr 'X' (w - 2) (h - 2) <|> vertLine
    in 
        horizLine <-> centerArea <-> horizLine

