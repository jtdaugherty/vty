{-# OPTIONS_HADDOCK hide #-}
module System.Console.ANSI.Unix (
#include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Common

import System.IO

import Data.List


#include "Common-Include.hs"


-- | The reference I used for the ANSI escape characters in this module was <http://en.wikipedia.org/wiki/ANSI_escape_sequences>.
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

colorToCode :: Color -> Int
colorToCode color = case color of
    Black   -> 0
    Red     -> 1
    Green   -> 2
    Yellow  -> 3
    Blue    -> 4
    Magenta -> 5
    Cyan    -> 6
    White   -> 7

sgrToCode :: SGR -> Int
sgrToCode sgr = case sgr of
    Reset -> 0
    SetConsoleIntensity intensity -> case intensity of
        BoldIntensity   -> 1
        FaintIntensity  -> 2
        NormalIntensity -> 22
    SetItalicized True  -> 3
    SetItalicized False -> 23
    SetUnderlining underlining -> case underlining of
        SingleUnderline -> 4
        DoubleUnderline -> 21
        NoUnderline     -> 24
    SetBlinkSpeed blink_speed -> case blink_speed of
        SlowBlink   -> 5
        RapidBlink  -> 6
        NoBlink     -> 25
    SetVisible False -> 8
    SetVisible True  -> 28
    SetSwapForegroundBackground True  -> 7
    SetSwapForegroundBackground False -> 27
    SetColor Foreground Dull color  -> 30 + colorToCode color
    SetColor Foreground Vivid color -> 90 + colorToCode color
    SetColor Background Dull color  -> 40 + colorToCode color
    SetColor Background Vivid color -> 100 + colorToCode color


cursorUpCode n = csi [n] "A"
cursorDownCode n = csi [n] "B"
cursorForwardCode n = csi [n] "C"
cursorBackwardCode n = csi [n] "D"

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n


cursorDownLineCode n = csi [n] "E"
cursorUpLineCode n = csi [n] "F"

hCursorDownLine h n = hPutStr h $ cursorDownLineCode n
hCursorUpLine h n = hPutStr h $ cursorUpLineCode n


setCursorColumnCode n = csi [n + 1] "G"
setCursorPositionCode n m = csi [n + 1, m + 1] "H"

hSetCursorColumn h n = hPutStr h $ setCursorColumnCode n
hSetCursorPosition h n m = hPutStr h $ setCursorPositionCode n m


clearFromCursorToScreenEndCode = csi [0] "J"
clearFromCursorToScreenBeginningCode = csi [1] "J"
clearScreenCode = csi [2] "J"

hClearFromCursorToScreenEnd h = hPutStr h clearFromCursorToScreenEndCode
hClearFromCursorToScreenBeginning h = hPutStr h clearFromCursorToScreenBeginningCode
hClearScreen h = hPutStr h clearScreenCode


clearFromCursorToLineEndCode = csi [0] "K"
clearFromCursorToLineBeginningCode = csi [1] "K"
clearLineCode = csi [2] "K"

hClearFromCursorToLineEnd h = hPutStr h clearFromCursorToLineEndCode
hClearFromCursorToLineBeginning h = hPutStr h clearFromCursorToLineBeginningCode
hClearLine h = hPutStr h clearLineCode


scrollPageUpCode n = csi [n] "S"
scrollPageDownCode n = csi [n] "T"

hScrollPageUp h n = hPutStr h $ scrollPageUpCode n
hScrollPageDown h n = hPutStr h $ scrollPageDownCode n


setSGRCode sgrs = csi (map sgrToCode sgrs) "m"

hSetSGR h sgrs = hPutStr h $ setSGRCode sgrs


hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode


-- | Thanks to Brandon S. Allbery and Curt Sampson for pointing me in the right direction on xterm title setting on haskell-cafe.
-- The "0" signifies that both the title and "icon" text should be set: i.e. the text for the window in the Start bar (or similar)
-- as well as that in the actual window title.  This is chosen for consistent behaviour between Unixes and Windows.
setTitleCode title = "\ESC]0;" ++ filter (/= '\007') title ++ "\007"

hSetTitle h title = hPutStr h $ setTitleCode title