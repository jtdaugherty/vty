module System.Console.ANSI.Windows.Emulator where

import System.Console.ANSI.Windows.Foreign


getScreenRectangle :: IO SMALL_RECT
getScreenRectangle = do
    handle <- getStdInputHandle
    fmap csbi_window $ getConsoleScreenBufferInfo handle


adjustCursorPosition :: (Int -> Int) -> (Int -> Int) -> IO ()
adjustCursorPosition change_x change_y = do
    handle <- getStdInputHandle
    screen_buffer_info <- getConsoleScreenBufferInfo handle
    let (CCoord x y) = csbi_cursor_position screen_buffer_info
        cursor_pos' = CCoord (change_x x) (change_y y)
    setConsoleCursorPosition handle cursor_pos

cursorUp n = adjustCursorPosition id (-n)
cursorDown n = adjustCursorPosition id (+n)
cursorForward n = adjustCursorPosition (+n) id
cursorBackward n = adjustCursorPosition (-n) id


adjustLine :: (Int -> Int) -> IO ()
adjustLine change_y = do
    left <- fmap rect_left $ getScreenRectangle
    adjustCursorPosition (const left) change_y

nextLine n = adjustLine (+n)
previousLine n = adjustLine (-n)


setColumn n = adjustCursorPosition (const n) id

setPosition x y = adjustCursorPosition (const x) (const y)


clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: IO ()

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: IO ()


scrollPage :: Int -> IO ()
scrollPage new_origin_y = do
    handle <- getStdInputHandle
    window <- getScreenRectangle
    let fill = CHAR_INFO ' ' undefined
    scrollConsoleScreenBuffer handle window Nothing (COORD (rect_left window) n) fill

scrollPageUp n = scrollPage (negate n)
scrollPageDown n = scrollPage n


setSGR :: ANSISGR -> IO ()


changeCursorVisibility :: Bool -> IO ()
changeCursorVisibility cursor_visible = do
    handle <- getStdInputHandle
    cursor_info <- getConsoleCursorInfo handle
    setConsoleCursorInfo handle (cursor_info { cci_cursor_visible = cursor_visible })

hideCursor = changeCursorVisibility False
showCursor = changeCursorVisibility True
    