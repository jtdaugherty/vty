{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- Copyright Corey O'Connor
module Graphics.Vty.Input.Data where

-- |Representations of non-modifier keys.
data Key = KEsc | KFun Int | KBackTab | KPrtScr | KPause | KASCII Char | KBS | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin |  KNP5 | KUp | KMenu
         | KLeft | KDown | KRight | KEnter
    deriving (Eq,Show,Ord)

-- |Modifier keys.  Key codes are interpreted such that users are more likely to
-- have Meta than Alt; for instance on the PC Linux console, 'MMeta' will
-- generally correspond to the physical Alt key.
data Modifier = MShift | MCtrl | MMeta | MAlt
    deriving (Eq,Show,Ord)

-- |Mouse buttons.  Not yet used.
data Button = BLeft | BMiddle | BRight
    deriving (Eq,Show,Ord)

-- |Generic events.
data Event = EvKey Key [Modifier] | EvMouse Int Int Button [Modifier]
           | EvResize Int Int
    deriving (Eq,Show,Ord)

data KClass = Valid Key [Modifier] | Invalid | Prefix | MisPfx Key [Modifier] [Char]
    deriving(Show)

map_to_legacy_table :: [(String, Event)] -> [(String, (Key, [Modifier]))]
map_to_legacy_table = map f
    where f (s, EvKey k mods) = (s, (k, mods))
          f _                 = error "no mapping for mouse or resize events"

caps_table :: [(String, Event)]
caps_table =
    [ ("khome", EvKey KHome     [])
    , ("kend",  EvKey KEnd      [])
    , ("cbt",   EvKey KBackTab  [])
    , ("kcud1", EvKey KDown     [])
    , ("kcuu1", EvKey KUp       [])
    , ("kcuf1", EvKey KRight    [])
    , ("kcub1", EvKey KLeft     [])
    , ("kLFT",  EvKey KLeft     [MShift])
    , ("kRIT",  EvKey KRight    [MShift])
    ]

