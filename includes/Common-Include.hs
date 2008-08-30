cursorUp, cursorDown, cursorForward, cursorBackward :: Int -- ^ Number of lines or characters to move
                                                    -> IO ()

nextLine, previousLine :: Int -- ^ Number of lines to move
                       -> IO ()

setColumn :: Int -- ^ 1-based column to move to
          -> IO ()

setPosition :: Int -- ^ 1-based row to move to
            -> Int -- ^ 1-based column to move to
            -> IO ()

clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: IO ()

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: IO ()

scrollPageUp, scrollPageDown :: Int -- ^ Number of lines to scroll by
                             -> IO ()

setSGR :: ANSISGR -> IO ()

hideCursor, showCursor :: IO ()