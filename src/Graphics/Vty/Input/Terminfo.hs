module Graphics.Vty.Input.Terminfo where

import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal

import System.Console.Terminfo

caps_classify_table :: Terminal -> [(String,Event)] -> [(String,Event)]
caps_classify_table terminal table = [(x,y) | (Just x,y) <- map extract_cap table]
    where extract_cap = first (getCapability terminal . tiGetStr)
