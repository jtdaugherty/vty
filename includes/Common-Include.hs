hCursorUp, hCursorDown, hCursorForward, hCursorBackward :: Handle
                                                        -> Int -- ^ Number of lines or characters to move
                                                        -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward :: Int -- ^ Number of lines or characters to move
                                                    -> IO ()

cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout


hNextLine, hPreviousLine :: Handle
                         -> Int -- ^ Number of lines to move
                         -> IO ()
nextLine, previousLine :: Int -- ^ Number of lines to move
                       -> IO ()

nextLine = hNextLine stdout
previousLine = hPreviousLine stdout


hSetColumn :: Handle
           -> Int -- ^ 0-based column to move to
           -> IO ()
setColumn :: Int -- ^ 0-based column to move to
          -> IO ()

setColumn = hSetColumn stdout


hSetPosition :: Handle
             -> Int -- ^ 0-based row to move to
             -> Int -- ^ 0-based column to move to
             -> IO ()
setPosition :: Int -- ^ 0-based row to move to
            -> Int -- ^ 0-based column to move to
            -> IO ()

setPosition = hSetPosition stdout


hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen :: Handle
                                                                             -> IO ()
clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: IO ()

clearFromCursorToScreenEnd = hClearFromCursorToScreenEnd stdout
clearFromCursorToScreenBeginning = hClearFromCursorToScreenBeginning stdout
clearScreen = hClearScreen stdout


hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine :: Handle
                                                                       -> IO ()
clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: IO ()

clearFromCursorToLineEnd = hClearFromCursorToLineEnd stdout
clearFromCursorToLineBeginning = hClearFromCursorToLineBeginning stdout
clearLine = hClearLine stdout


-- | Scroll the displayed information up or down the terminal: not widely supported
hScrollPageUp, hScrollPageDown :: Handle
                               -> Int -- ^ Number of lines to scroll by
                               -> IO ()
-- | Scroll the displayed information up or down the terminal: not widely supported
scrollPageUp, scrollPageDown :: Int -- ^ Number of lines to scroll by
                             -> IO ()

scrollPageUp = hScrollPageUp stdout
scrollPageDown = hScrollPageDown stdout


-- | Set the Select Graphic Rendition mode
hSetSGR :: Handle
        -> ANSISGR -- ^ Mode: this is applied to the current console SGR mode
        -> IO ()
-- | Set the Select Graphic Rendition mode
setSGR :: ANSISGR -- ^ Mode: this is applied to the current console SGR mode
       -> IO ()

setSGR = hSetSGR stdout


hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()

hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout