-- | Input mappings for ANSI/VT100/VT50 terminals that is missing from
-- terminfo.
--
-- Or that are sent regardless of terminfo by terminal emulators. EG:
-- Terminal emulators will often use VT50 input bytes regardless of
-- declared terminal type. This provides compatibility with programs
-- that don't follow terminfo.
module Graphics.Vty.Input.Terminfo.ANSIVT
  ( classifyTable
  )
where

import Graphics.Vty.Input.Events

-- | Encoding for navigation keys.
navKeys0 :: ClassifyMap
navKeys0 =
    [ k "G" KCenter
    , k "P" KPause
    , k "A" KUp
    , k "B" KDown
    , k "C" KRight
    , k "D" KLeft
    , k "H" KHome
    , k "F" KEnd
    , k "E" KBegin
    ]
    where k c s = ("\ESC["++c,EvKey s [])

-- | encoding for shift, meta and ctrl plus arrows/home/end
navKeys1 :: ClassifyMap
navKeys1 =
   [("\ESC[" ++ charCnt ++ show mc++c,EvKey s m)
    | charCnt <- ["1;", ""], -- we can have a count or not
    (m,mc) <- [([MShift],2::Int), ([MCtrl],5), ([MMeta],3),
               -- modifiers and their codes
               ([MShift, MCtrl],6), ([MShift, MMeta],4)],
    -- directions and their codes
    (c,s) <- [("A", KUp), ("B", KDown), ("C", KRight), ("D", KLeft), ("H", KHome), ("F", KEnd)]
   ]

-- | encoding for ins, del, pageup, pagedown, home, end
navKeys2 :: ClassifyMap
navKeys2 =
    let k n s = ("\ESC["++show n++"~",EvKey s [])
    in zipWith k [2::Int,3,5,6,1,4]
                 [KIns,KDel,KPageUp,KPageDown,KHome,KEnd]

-- | encoding for ctrl + ins, del, pageup, pagedown, home, end
navKeys3 :: ClassifyMap
navKeys3 =
    let k n s = ("\ESC["++show n++";5~",EvKey s [MCtrl])
    in zipWith k [2::Int,3,5,6,1,4]
                 [KIns,KDel,KPageUp,KPageDown,KHome,KEnd]

-- | encoding for shift plus function keys
--
-- According to
--
--  * http://aperiodic.net/phil/archives/Geekery/term-function-keys.html
--
-- This encoding depends on the terminal.
functionKeys1 :: ClassifyMap
functionKeys1 =
    let f ff nrs m = [ ("\ESC["++show n++"~",EvKey (KFun $ n-head nrs+ff) m) | n <- nrs ] in
    concat [f 1 [25,26] [MShift], f 3 [28,29] [MShift], f 5 [31..34] [MShift] ]

-- | encoding for meta plus char
--
-- 1. removed 'ESC' from second list due to duplication with
-- "special_support_keys".
-- 2. removed '[' from second list due to conflict with 7-bit encoding
-- for ESC. Whether meta+[ is the same as ESC should examine km and
-- current encoding.
-- 3. stopped enumeration at '~' instead of '\DEL'. The latter is mapped
-- to KBS by special_support_keys.
functionKeys2 :: ClassifyMap
functionKeys2 = [ ('\ESC':[x],EvKey (KChar x) [MMeta])
                  | x <- '\t':[' ' .. '~']
                  , x /= '['
                  ]

classifyTable :: [ClassifyMap]
classifyTable =
    [ navKeys0
    , navKeys1
    , navKeys2
    , navKeys3
    , functionKeys1
    , functionKeys2
    ]
