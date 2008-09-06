module System.Console.ANSI.Unix (
#include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Common

import System.IO

import Data.List


#include "Common-Include.hs"


-- | The reference I used for the escape characters in this module was http://en.wikipedia.org/wiki/ANSI_escape_sequences
csi :: Handle -> [Int] -> String -> IO ()
csi handle args code = hPutStr handle $ "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

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


hCursorUp h n = csi h [n] "A"
hCursorDown h n = csi h [n] "B"
hCursorForward h n = csi h [n] "C"
hCursorBackward h n = csi h [n] "D"

hNextLine h n = csi h [n] "E"
hPreviousLine h n = csi h [n] "F"

hSetColumn h n = csi h [n + 1] "G"

hSetPosition h n m = csi h [n + 1, m + 1] "H"

hClearFromCursorToScreenEnd h = csi h [0] "J"
hClearFromCursorToScreenBeginning h = csi h [1] "J"
hClearScreen h = csi h [2] "J"

hClearFromCursorToLineEnd h = csi h [0] "K"
hClearFromCursorToLineBeginning h = csi h [1] "K"
hClearLine h = csi h [2] "K"

hScrollPageUp h n = csi h [n] "S"
hScrollPageDown h n = csi h [n] "T"

hSetSGR h sgr = csi h [ansiSGRToCode sgr] "m"

hHideCursor h = csi h [] "?25l"
hShowCursor h = csi h [] "?25h"