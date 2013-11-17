module Graphics.Vty.Input.Terminfo where

import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal

import System.Console.Terminfo

caps_classify_table :: Terminal -> [(String,Event)] -> [(String,Event)]
caps_classify_table terminal table = [(x,y) | (Just x,y) <- map extract_cap table]
    where extract_cap = first (getCapability terminal . tiGetStr)

classify_table_for_term :: Terminal -> ClassifyTable
classify_table_for_term term =
    let caps_legacy_table = map_to_legacy_table $ caps_classify_table term keys_from_caps_table
    in concat $ caps_legacy_table : ansi_classify_table
