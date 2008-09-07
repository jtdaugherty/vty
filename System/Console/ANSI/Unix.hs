{-# OPTIONS_HADDOCK hide #-}
module System.Console.ANSI.Unix (
#include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Common

import System.IO

import Data.List


#include "Common-Include.hs"


-- | The reference I used for the escape characters in this module was http://en.wikipedia.org/wiki/ANSI_escape_sequences
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

ansiColorToCode :: ANSIColor -> Int
ansiColorToCode color = case color of
    Black   -> 0
    Red     -> 1
    Green   -> 2
    Yellow  -> 3
    Blue    -> 4
    Magenta -> 5
    Cyan    -> 6
    White   -> 7

ansiSGRToCode :: ANSISGR -> Int
ansiSGRToCode sgr = case sgr of
    Reset -> 0
    BoldIntensity   -> 1
    FaintIntensity  -> 2
    NormalIntensity -> 22
    Italic -> 3
    SingleUnderline -> 4
    DoubleUnderline -> 21
    NoUnderline     -> 24
    SlowBlink   -> 5
    RapidBlink  -> 6
    NoBlink     -> 25
    Conceal -> 8
    Reveal  -> 28
    SwapForegroundBackground        -> 7
    DontSwapForegroundBackground    -> 27
    ForegroundNormalIntensity color -> 30 + ansiColorToCode color
    ForegroundHighIntensity color   -> 90 + ansiColorToCode color
    BackgroundNormalIntensity color -> 40 + ansiColorToCode color
    BackgroundHighIntensity color   -> 100 + ansiColorToCode color


cursorUpCode n = csi [n] "A"
cursorDownCode n = csi [n] "B"
cursorForwardCode n = csi [n] "C"
cursorBackwardCode n = csi [n] "D"

hCursorUp h n = hPutStr h $ cursorUpCode n
hCursorDown h n = hPutStr h $ cursorDownCode n
hCursorForward h n = hPutStr h $ cursorForwardCode n
hCursorBackward h n = hPutStr h $ cursorBackwardCode n


nextLineCode n = csi [n] "E"
previousLineCode n = csi [n] "F"

hNextLine h n = hPutStr h $ nextLineCode n
hPreviousLine h n = hPutStr h $ previousLineCode n


setColumnCode n = csi [n + 1] "G"
setPositionCode n m = csi [n + 1, m + 1] "H"

hSetColumn h n = hPutStr h $ setColumnCode n
hSetPosition h n m = hPutStr h $ setPositionCode n m


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


setSGRCode sgr = csi [ansiSGRToCode sgr] "m"

hSetSGR h sgr = hPutStr h $ setSGRCode sgr


hideCursorCode = csi [] "?25l"
showCursorCode = csi [] "?25h"

hHideCursor h = hPutStr h hideCursorCode
hShowCursor h = hPutStr h showCursorCode