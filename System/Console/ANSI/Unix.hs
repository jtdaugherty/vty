module System.Console.ANSI.Unix (
#include "Exports-Include.hs"
    ) where

#include "Common-Include.hs"


-- | The reference I used for the escape characters in this module was http://en.wikipedia.org/wiki/ANSI_escape_sequences
csi :: [Int] -> String -> IO ()
csi args code = putStr $ "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code


cursorUp n = csi [n] "A"
cursorDown n = csi [n] "B"
cursorForward n = csi [n] "C"
cursorBackward n = csi [n] "D"

nextLine n = csi [n] "E"
previousLine n = csi [n] "F"

setColumn n = csi [n] "G"

setPosition n m = csi [n, m] "H"

clearFromCursorToScreenEnd = csi [0] "J"
clearFromCursorToScreenBeginning = csi [1] "J"
clearScreen = csi [2] "J"

clearFromCursorToLineEnd = csi [0] "K"
clearFromCursorToLineBeginning = csi [1] "K"
clearLine = csi [2] "K"

scrollPageUp n = csi [n] "S"
scrollPageDown n = csi [n] "T"

setSGR sgr = csi [ansiSGRToCode n] "m"

hideCursor = csi [] "?25l"
showCursor = csi [] "?25h"


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