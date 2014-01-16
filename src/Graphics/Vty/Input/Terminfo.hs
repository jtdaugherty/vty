module Graphics.Vty.Input.Terminfo where

import Graphics.Vty.Input.Events
import qualified Graphics.Vty.Input.Terminfo.VT100 as VT100
import qualified Graphics.Vty.Input.Terminfo.XTerm7Bit as XTerm7Bit

import System.Console.Terminfo

-- | queries the terminal for all capability based input sequences then adds on a terminal dependent
-- input sequence mapping.
--
-- For reference see:
--
-- * http://vimdoc.sourceforge.net/htmldoc/term.html
--
-- * vim74/src/term.c
--
-- * http://invisible-island.net/vttest/
--
-- * http://aperiodic.net/phil/archives/Geekery/term-function-keys.html
--
-- This is painful. Terminfo is incomplete. The vim source implies that terminfo is also incorrect.
-- Vty assumes that the an internal terminfo table added to the system provided terminfo table is
-- correct.
--
-- 1. build terminfo table for all caps. Missing caps are not added.
--
-- 2. add tables for visible chars, esc, del plus ctrl and meta.
--
-- 3. add internally defined table for given terminal type.
--
-- The override is currently done in 'compile'. Which is a bit odd.
--
-- \todo terminfo meta is not supported.
-- \todo no 8bit
classify_table_for_term :: String -> Terminal -> ClassifyTableV1
classify_table_for_term term_name term =
    concatMap map_to_legacy_table
              $ caps_classify_table term keys_from_caps_table
                : visible_chars
                : ctrl_chars
                : ctrl_meta_chars
                : special_support_keys
                : internal_tables term_name

caps_classify_table :: Terminal -> [(String,Event)] -> [(String,Event)]
caps_classify_table terminal table = [(x,y) | (Just x,y) <- map extract_cap table]
    where extract_cap = first (getCapability terminal . tiGetStr)

internal_tables term_name
    | ANSI.supports term_name = ANSI.tables
    | XTerm7Bit.supports term_name = XTerm7Bit.tables
    | otherwise = []

-- | Visible characters in the ISO-8859-1 and UTF-8 common set.
--
-- we limit to < 0xC1. The UTF8 sequence detector will catch all values 0xC2 and above before this
-- classify table is reached.
--
-- TODO: resolve
-- 1. start at ' '. The earlier characters are all ctrl_char_keys
visible_chars :: ClassifyTable
visible_chars = [ ([x], EvKey (KChar x) [])
                 | x <- [' ' .. toEnum 0xC1]
                 ]

-- | Non visible cahracters in the ISO-8859-1 and UTF-8 common set translated to ctrl + char.
--
-- \todo resolve CTRL-i is the same as tab
ctrl_chars :: ClassifyTable
ctrl_chars =
    [ ([toEnum x],EvKey (KChar y) [MCtrl])
    | (x,y) <- zip ([0..31]) ('@':['a'..'z']++['['..'_']),
      y /= 'i' -- Resolve issue #3 where CTRL-i hides TAB.
    ]

-- | Ctrl+Meta+Char
ctrl_meta_chars :: ClassifyTable
ctrl_meta_chars = map (\(s,EvKey c m) -> ('\ESC':s, EvKey c (MMeta:m)))

-- | esc, meta esc, delete, meta delete, enter, meta enter
special_support_keys :: ClassifyTable
special_support_keys =
    [ -- special support for ESC
      ("\ESC",EvKey KEsc []), ("\ESC\ESC",EvKey KEsc [MMeta])
    -- Special support for backspace
    , ("\DEL",EvKey KBS []), ("\ESC\DEL",EvKey KBS [MMeta])
    -- Special support for Enter
    , ("\ESC\^J",EvKey KEnter [MMeta]), ("\^J",EvKey KEnter [])
    -- explicit support for tab
    , ("\t", EvKey KChar '\t' []
    ]

-- | classify table directly generated from terminfo cap strings
--
-- these are:
--
-- * ka1 - keypad up-left
--
-- * ka3 - keypad up-right
--
-- * kb2 - keypad center
--
-- * kbs - keypad backspace
--
-- * kbeg - begin
--
-- * kcbt - back tab
--
-- * kc1 - keypad left-down
-- 
-- * kc3 - keypad right-down
--
-- * kdch1 - delete
--
-- * kcud1 - down
--
-- * kend - end
-- 
-- * kent - enter
--
-- * kf0 - kf63 - function keys
--
-- * khome - KHome
--
-- * kich1 - insert
--
-- * kcub1 - left
--
-- * knp - next page (page down)
--
-- * kpp - previous page (page up)
--
-- * kcuf1 - right
--
-- * kDC - shift delete
--
-- * kEND - shift end
--
-- * kHOM - shift home
--
-- * kIC - shift insert
--
-- * kLFT - shift left
--
-- * kRIT - shift right
--
-- * kcuu1 - up
keys_from_caps_table :: ClassifyTable
keys_from_caps_table =
    [ ("ka1",   EvKey KUpLeft    [])
    , ("ka3",   EvKey KUpRight   [])
    , ("kb2",   EvKey KCenter    [])
    , ("kbs",   EvKey KBS        [])
    , ("kbeg",  EvKey KBegin     [])
    , ("kcbt",  EvKey KBackTab   [])
    , ("kc1",   EvKey KDownLeft  [])
    , ("kc3",   EvKey KDownRight [])
    , ("kdch1", EvKey KDel       [])
    , ("kcud1", EvKey KDown      [])
    , ("kend",  EvKey KEnd       [])
    , ("kent",  EvKey KEnter     [])
    , ("khome", EvKey KHome      [])
    , ("kich1", EvKey KIns       [])
    , ("kcub1", EvKey KLeft      [])
    , ("knp",   EvKey KPageDown  [])
    , ("kpp",   EvKey KPageUp    [])
    , ("kcuf1", EvKey KRight     [])
    , ("kDC",   EvKey KDel       [MShift])
    , ("kEND",  EvKey KEnd       [MShift])
    , ("kHOM",  EvKey KHome      [MShift])
    , ("kIC",   EvKey KIns       [MShift])
    , ("kLFT",  EvKey KLeft      [MShift])
    , ("kRIT",  EvKey KRight     [MShift])
    , ("kcuu1", EvKey KUp        [])
    ] ++ function_key_caps_table

-- | cap names for function keys
function_key_caps_table :: ClassifyTable
function_key_caps_table = flip map [0..63] $ \n -> ("kf" ++ show n, EvKey (KFun n) [])
