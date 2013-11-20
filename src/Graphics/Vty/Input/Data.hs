-- Copyright Corey O'Connor
module Graphics.Vty.Input.Data ( module Graphics.Vty.Input.Data
                               , module Graphics.Vty.Input.Data.ANSI
                               , module Graphics.Vty.Input.Events
                               )
where

import Graphics.Vty.Input.Data.ANSI
import Graphics.Vty.Input.Events

map_to_legacy_table :: ClassifyTable -> ClassifyTableV1
map_to_legacy_table = map f
    where f (s, EvKey k mods) = (s, (k, mods))
          f _                 = error "no mapping for mouse or resize events"

-- | classify table directly generated from terminfo cap strings
keys_from_caps_table :: ClassifyTable
keys_from_caps_table =
    [ ("khome", EvKey KHome     [])
    , ("kend",  EvKey KEnd      [])
    , ("cbt",   EvKey KBackTab  [])
    , ("kcud1", EvKey KDown     [])
    , ("kcuu1", EvKey KUp       [])
    , ("kcuf1", EvKey KRight    [])
    , ("kcub1", EvKey KLeft     [])
    , ("kLFT",  EvKey KLeft     [MShift])
    , ("kRIT",  EvKey KRight    [MShift])
    ] ++ function_key_caps_table

-- | cap names for function keys
function_key_caps_table :: ClassifyTable
function_key_caps_table = flip map [0..63] $ \n -> ("kf" ++ show n, EvKey (KFun n) [])

