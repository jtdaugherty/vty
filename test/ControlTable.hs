module Main where

import Graphics.Vty.ControlStrings

import System.Console.Terminfo

main = do
    terminal <- setupTermFromEnv 
    control_table <- init_control_table terminal
    putStrLn $ "ANSI terminal show cursor string: " ++ show cvis
    putStrLn $ "Current terminal show cursor string: " ++ show (show_cursor_str control_table)
    putStrLn $ "ANSI terminal hide cursor string: " ++ show civis
    putStrLn $ "Current terminal hide cursor string: " ++ show (hide_cursor_str control_table)

