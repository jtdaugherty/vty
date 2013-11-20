module Graphics.Vty.Input.Events where

-- | Representations of non-modifier keys.
--
-- KFun is indexed from 0 to 63. Range of supported FKeys varies by terminal and keyboard.
data Key = KEsc | KFun Int | KBackTab | KPrtScr | KPause | KASCII Char | KBS | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin |  KNP5 | KUp | KMenu
         | KLeft | KDown | KRight | KEnter
    deriving (Eq,Show,Ord)

-- | Modifier keys.  Key codes are interpreted such that users are more likely to
-- have Meta than Alt; for instance on the PC Linux console, 'MMeta' will
-- generally correspond to the physical Alt key.
data Modifier = MShift | MCtrl | MMeta | MAlt
    deriving (Eq,Show,Ord)

-- | Mouse buttons.  Not yet used.
data Button = BLeft | BMiddle | BRight
    deriving (Eq,Show,Ord)

-- | Events.
data Event = EvKey Key [Modifier] | EvMouse Int Int Button [Modifier]
           | EvResize Int Int
    deriving (Eq,Show,Ord)

-- | representation of mapping from input bytes to key and modifier.
--
-- deprecated
type ClassifyTableV1 = [(String, (Key, [Modifier]))]

-- | representation of mapping from input bytes to event
type ClassifyTable = [(String, Event)]

