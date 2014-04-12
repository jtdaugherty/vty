module Main where

import Graphics.Vty.ControlStrings

import System.Console.Terminfo

main = do
    terminal <- setupTermFromEnv 
    controlTable <- init_controlTable terminal
    putStrLn $ "ANSI terminal show cursor string: " ++ show cvis
    putStrLn $ "Current terminal show cursor string: " ++ show (showCursorStr controlTable)
    putStrLn $ "ANSI terminal hide cursor string: " ++ show civis
    putStrLn $ "Current terminal hide cursor string: " ++ show (hideCursorStr controlTable)

