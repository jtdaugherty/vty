module System.Console.ANSI.Windows.Emulator where

import System.Console.ANSI.Common
import System.Console.ANSI.Windows.Foreign

import Data.Bits


#include "Common-Include.hs"


screenRectangle :: IO SMALL_RECT
screenRectangle = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    fmap csbi_window $ getConsoleScreenBufferInfo handle


adjustCursorPosition :: (SHORT -> SHORT) -> (SHORT -> SHORT) -> IO ()
adjustCursorPosition change_x change_y = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    screen_buffer_info <- getConsoleScreenBufferInfo handle
    let (COORD x y) = csbi_cursor_position screen_buffer_info
        cursor_pos' = COORD (change_x x) (change_y y)
    setConsoleCursorPosition handle cursor_pos'

cursorUp n = adjustCursorPosition id (\x -> x - fromIntegral n)
cursorDown n = adjustCursorPosition id (+ (fromIntegral n))
cursorForward n = adjustCursorPosition (+ (fromIntegral n)) id
cursorBackward n = adjustCursorPosition (\x -> x - fromIntegral n) id


adjustLine :: (SHORT -> SHORT) -> IO ()
adjustLine change_y = do
    left <- fmap rect_left $ screenRectangle
    adjustCursorPosition (const (fromIntegral left)) change_y

nextLine n = adjustLine (+ (fromIntegral n))
previousLine n = adjustLine (\x -> x - fromIntegral n)


setColumn n = adjustCursorPosition (const (fromIntegral n)) id

setPosition x y = adjustCursorPosition (const (fromIntegral x)) (const (fromIntegral y))


clearFromCursorToScreenEnd = undefined
clearFromCursorToScreenBeginning = undefined
clearScreen = undefined


clearFromCursorToLineEnd = undefined
clearFromCursorToLineBeginning = undefined
clearLine = undefined


scrollPage :: Int -> IO ()
scrollPage new_origin_y = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    screen_buffer_info <- getConsoleScreenBufferInfo handle
    let fill = CHAR_INFO (charToWCHAR ' ') (csbi_attributes screen_buffer_info)
        window = csbi_window screen_buffer_info
        origin = COORD (rect_left window) (rect_top window + fromIntegral new_origin_y)
    scrollConsoleScreenBuffer handle window Nothing origin fill

scrollPageUp n = scrollPage (negate n)
scrollPageDown n = scrollPage n


{-# INLINE applyANSIColorToAttribute #-}
applyANSIColorToAttribute :: WORD -> WORD -> WORD -> ANSIColor -> WORD -> WORD
applyANSIColorToAttribute rED gREEN bLUE color attribute = case color of
    Black   -> attribute'
    Red     -> attribute' .|. rED
    Green   -> attribute' .|. gREEN
    Yellow  -> attribute' .|. rED .|. gREEN
    Blue    -> attribute' .|. bLUE
    Magenta -> attribute' .|. rED .|. bLUE
    Cyan    -> attribute' .|. gREEN .|. bLUE
    White   -> attribute' .|. wHITE
  where
    wHITE = rED .|. gREEN .|. bLUE
    attribute' = attribute .&. (complement wHITE)

applyForegroundANSIColorToAttribute, applyBackgroundANSIColorToAttribute :: ANSIColor -> WORD -> WORD
applyForegroundANSIColorToAttribute = applyANSIColorToAttribute fOREGROUND_RED fOREGROUND_GREEN fOREGROUND_BLUE
applyBackgroundANSIColorToAttribute = applyANSIColorToAttribute bACKGROUND_RED bACKGROUND_GREEN bACKGROUND_BLUE

applyANSISGRToAttribute :: ANSISGR -> WORD -> WORD
applyANSISGRToAttribute sgr attribute = case sgr of
    Reset -> 0
    BoldIntensity   -> attribute .|. iNTENSITY
    FaintIntensity  -> attribute .&. (complement iNTENSITY) -- Not supported
    NormalIntensity -> attribute .&. (complement iNTENSITY)
    Italic -> attribute -- Not supported
    SingleUnderline -> attribute .|. cOMMON_LVB_UNDERSCORE -- Not supported
    DoubleUnderline -> attribute .|. cOMMON_LVB_UNDERSCORE
    NoUnderline     -> attribute .&. (complement cOMMON_LVB_UNDERSCORE)
    SlowBlink  -> attribute -- Not supported
    RapidBlink -> attribute -- Not supported
    NoBlink    -> attribute
    Conceal -> attribute -- Not supported
    Reveal  -> attribute
    SwapForegroundBackground     -> attribute .|. cOMMON_LVB_REVERSE_VIDEO
    DontSwapForegroundBackground -> attribute .&. (complement cOMMON_LVB_REVERSE_VIDEO)
    ForegroundNormalIntensity color -> applyForegroundANSIColorToAttribute color (attribute .&. (complement fOREGROUND_INTENSITY))
    ForegroundHighIntensity color   -> applyForegroundANSIColorToAttribute color (attribute .|. fOREGROUND_INTENSITY)
    BackgroundNormalIntensity color -> applyBackgroundANSIColorToAttribute color (attribute .&. (complement bACKGROUND_INTENSITY))
    BackgroundHighIntensity color   -> applyBackgroundANSIColorToAttribute color (attribute .|. bACKGROUND_INTENSITY)
  where
    iNTENSITY = fOREGROUND_INTENSITY .|. bACKGROUND_INTENSITY

setSGR sgr = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    screen_buffer_info <- getConsoleScreenBufferInfo handle
    let attribute = csbi_attributes screen_buffer_info
        attribute' = applyANSISGRToAttribute sgr attribute
    setConsoleTextAttribute handle attribute'


changeCursorVisibility :: Bool -> IO ()
changeCursorVisibility cursor_visible = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    cursor_info <- getConsoleCursorInfo handle
    setConsoleCursorInfo handle (cursor_info { cci_cursor_visible = cursor_visible })

hideCursor = changeCursorVisibility False
showCursor = changeCursorVisibility True
    