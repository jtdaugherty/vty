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

-- | classify table directly generated from terminfo cap strings
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


-- | classify table for ANSI terminals
ansi_classify_table :: [[([Char], (Key, [Modifier]))]]
ansi_classify_table =
 [ let k c s = ("\ESC["++c,(s,[])) in [ k "G" KNP5
                                      , k "P" KPause
                                      , k "A" KUp
                                      , k "B" KDown
                                      , k "C" KRight
                                      , k "D" KLeft
                                      , k "H" KHome
                                      , k "F" KEnd
                                      , k "E" KBegin
                                      ],

   -- Support for arrows and KHome/KEnd
   [("\ESC[" ++ charCnt ++ show mc++c,(s,m))
    | charCnt <- ["1;", ""], -- we can have a count or not
    (m,mc) <- [([MShift],2::Int), ([MCtrl],5), ([MMeta],3),
               ([MShift, MCtrl],6), ([MShift, MMeta],4)], -- modifiers and their codes
    (c,s) <- [("A", KUp), ("B", KDown), ("C", KRight), ("D", KLeft), ("H", KHome), ("F", KEnd)] -- directions and their codes
   ],

   let k n s = ("\ESC["++show n++"~",(s,[]))
   in zipWith k [2::Int,3,5,6,1,4]
                [KIns,KDel,KPageUp,KPageDown,KHome,KEnd],

   let k n s = ("\ESC["++show n++";5~",(s,[MCtrl]))
   in zipWith k [2::Int,3,5,6,1,4]
                [KIns,KDel,KPageUp,KPageDown,KHome,KEnd],

   -- Support for simple characters.
   [ (x:[],(KASCII x,[])) | x <- map toEnum [0..255] ],

   -- Support for function keys (should use terminfo)
   [ ("\ESC[["++[toEnum(64+i)],(KFun i,[])) | i <- [1..5] ],
   let f ff nrs m = [ ("\ESC["++show n++"~",(KFun (n-(nrs!!0)+ff), m)) | n <- nrs ] in
   concat [ f 6 [17..21] [], f 11 [23,24] [], f 1 [25,26] [MShift], f 3 [28,29] [MShift], f 5 [31..34] [MShift] ],
   [ ('\ESC':[x],(KASCII x,[MMeta])) | x <- '\ESC':'\t':[' ' .. '\DEL'] ],

   -- Ctrl+Char
   [ ([toEnum x],(KASCII y,[MCtrl]))
      | (x,y) <- zip ([0..31]) ('@':['a'..'z']++['['..'_']),
        y /= 'i' -- Resolve issue #3 where CTRL-i hides TAB.
   ],

   -- Ctrl+Meta+Char
   [ ('\ESC':[toEnum x],(KASCII y,[MMeta,MCtrl])) | (x,y) <- zip [0..31] ('@':['a'..'z']++['['..'_']) ],

   -- Special support
   [ -- special support for ESC
     ("\ESC",(KEsc,[])) , ("\ESC\ESC",(KEsc,[MMeta])),

     -- Special support for backspace
     ("\DEL",(KBS,[])), ("\ESC\DEL",(KBS,[MMeta])),

     -- Special support for Enter
     ("\ESC\^J",(KEnter,[MMeta])), ("\^J",(KEnter,[])) ]
 ]
