module System.Console.ANSI.Windows.Emulator (
        #include "Exports-Include.hs"
    ) where

import System.Console.ANSI.Common
import System.Console.ANSI.Windows.Foreign

import Data.Bits


#include "Common-Include.hs"


screenRectangle :: IO SMALL_RECT
screenRectangle = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    fmap csbi_window $ getConsoleScreenBufferInfo handle


adjustCursorPosition :: (SHORT -> SHORT -> SHORT) -> (SHORT -> SHORT -> SHORT) -> IO ()
adjustCursorPosition change_x change_y = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    screen_buffer_info <- getConsoleScreenBufferInfo handle
    let window = csbi_window screen_buffer_info
        (COORD x y) = csbi_cursor_position screen_buffer_info
        cursor_pos' = COORD (change_x (rect_left window) x) (change_y (rect_top window) y)
    setConsoleCursorPosition handle cursor_pos'

cursorUp n       = adjustCursorPosition (\_ x -> x) (\_ y -> y - fromIntegral n)
cursorDown n     = adjustCursorPosition (\_ x -> x) (\_ y -> y + fromIntegral n)
cursorForward n  = adjustCursorPosition (\_ x -> x + fromIntegral n) (\_ y -> y)
cursorBackward n = adjustCursorPosition (\_ x -> x - fromIntegral n) (\_ y -> y)


adjustLine :: (SHORT -> SHORT -> SHORT) -> IO ()
adjustLine change_y = do
    adjustCursorPosition (\window_left _ -> window_left) change_y

nextLine n     = adjustLine (\_ y -> y + fromIntegral n)
previousLine n = adjustLine (\_ y -> y - fromIntegral n)


setColumn x = adjustCursorPosition (\window_left _ -> window_left + fromIntegral x) (\_ y -> y)

setPosition y x = adjustCursorPosition (\window_left _ -> window_left + fromIntegral x) (\window_top _ -> window_top + fromIntegral y)


clearChar :: WCHAR
clearChar = charToWCHAR ' '

clearAttribute :: WORD
clearAttribute = 0

clearScreenFraction :: (SMALL_RECT -> COORD -> (DWORD, COORD)) -> IO ()
clearScreenFraction fraction_finder = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    screen_buffer_info <- getConsoleScreenBufferInfo handle
    
    let window = csbi_window screen_buffer_info
        cursor_pos = csbi_cursor_position screen_buffer_info
        (fill_length, fill_cursor_pos) = fraction_finder window cursor_pos
    
    fillConsoleOutputCharacter handle clearChar fill_length fill_cursor_pos
    fillConsoleOutputAttribute handle clearAttribute fill_length fill_cursor_pos
    return ()

clearFromCursorToScreenEnd = clearScreenFraction go
  where
    go window cursor_pos = (fromIntegral fill_length, cursor_pos)
      where
        size_x = rect_width window
        size_y = rect_bottom window - coord_y cursor_pos
        line_remainder = size_x - coord_x cursor_pos
        fill_length = size_x * size_y + line_remainder

clearFromCursorToScreenBeginning = clearScreenFraction go
  where
    go window cursor_pos = (fromIntegral fill_length, rect_top_left window)
      where
        size_x = rect_width window
        size_y = coord_y cursor_pos - rect_top window
        line_remainder = coord_x cursor_pos
        fill_length = size_x * size_y + line_remainder

clearScreen = clearScreenFraction go
  where
    go window _ = (fromIntegral fill_length, rect_top_left window)
      where
        size_x = rect_width window
        size_y = rect_height window
        fill_length = size_x * size_y

clearFromCursorToLineEnd = clearScreenFraction go
  where
    go window cursor_pos = (fromIntegral (rect_right window - coord_x cursor_pos), cursor_pos)

clearFromCursorToLineBeginning = clearScreenFraction go
  where
    go window cursor_pos = (fromIntegral (coord_x cursor_pos), cursor_pos { coord_x = rect_left window })

clearLine = clearScreenFraction go
  where
    go window cursor_pos = (fromIntegral (rect_width window), cursor_pos { coord_x = rect_left window })


scrollPage :: Int -> IO ()
scrollPage new_origin_y = do
    handle <- getStdHandle sTD_OUTPUT_HANDLE
    screen_buffer_info <- getConsoleScreenBufferInfo handle
    let fill = CHAR_INFO clearChar clearAttribute
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

swapForegroundBackgroundColors :: WORD -> WORD
swapForegroundBackgroundColors attribute = clean_attribute .|. foreground_attribute' .|. background_attribute'
  where
    foreground_attribute = attribute .&. fOREGROUND_INTENSE_WHITE
    background_attribute = attribute .&. bACKGROUND_INTENSE_WHITE
    clean_attribute = attribute .&. complement (fOREGROUND_INTENSE_WHITE .|. bACKGROUND_INTENSE_WHITE)
    foreground_attribute' = background_attribute `shiftR` 4
    background_attribute' = foreground_attribute `shiftL` 4

applyANSISGRToAttribute :: ANSISGR -> WORD -> WORD
applyANSISGRToAttribute sgr attribute = case sgr of
    Reset -> fOREGROUND_WHITE
    BoldIntensity   -> attribute .|. iNTENSITY
    FaintIntensity  -> attribute .&. (complement iNTENSITY) -- Not supported
    NormalIntensity -> attribute .&. (complement iNTENSITY)
    Italic -> attribute -- Not supported
    SingleUnderline -> attribute .|. cOMMON_LVB_UNDERSCORE -- Not supported, since cOMMON_LVB_UNDERSCORE seems to have no effect
    DoubleUnderline -> attribute .|. cOMMON_LVB_UNDERSCORE -- Not supported, since cOMMON_LVB_UNDERSCORE seems to have no effect
    NoUnderline     -> attribute .&. (complement cOMMON_LVB_UNDERSCORE)
    SlowBlink  -> attribute -- Not supported
    RapidBlink -> attribute -- Not supported
    NoBlink    -> attribute
    Conceal -> attribute -- Not supported
    Reveal  -> attribute
    -- The cOMMON_LVB_REVERSE_VIDEO doesn't actually appear to have any affect on the colors being displayed, so the emulator
    -- just uses it to carry information and implements the color-swapping behaviour itself. Bit of a hack, I guess :-)
    SwapForegroundBackground     ->
        -- Check if the color-swapping flag is already set
        if attribute .&. cOMMON_LVB_REVERSE_VIDEO /= 0
         then attribute
         else swapForegroundBackgroundColors attribute .|. cOMMON_LVB_REVERSE_VIDEO
    DontSwapForegroundBackground ->
        -- Check if the color-swapping flag is already not set
        if attribute .&. cOMMON_LVB_REVERSE_VIDEO == 0
         then attribute
         else swapForegroundBackgroundColors attribute .&. (complement cOMMON_LVB_REVERSE_VIDEO)
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
    