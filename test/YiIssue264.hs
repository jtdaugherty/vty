module Main where

import Graphics.Vty
import Control.Exception

catchLog = handle (\except -> do putStrLn $ show (except :: IOException))

main = do
  vty <- mkVty
  catchLog $ update vty pic { pImage = empty, pCursor = NoCursor }
  catchLog $ update vty pic { pImage = empty, pCursor = NoCursor }
  shutdown vty

