hCursorUp, hCursorDown, hCursorForward, hCursorBackward :: Handle
                                                        -> Int -- ^ Number of lines or characters to move
                                                        -> IO ()
cursorUp, cursorDown, cursorForward, cursorBackward :: Int -- ^ Number of lines or characters to move
                                                    -> IO ()
cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode :: Int -- ^ Number of lines or characters to move
                                                                    -> String

cursorUp = hCursorUp stdout
cursorDown = hCursorDown stdout
cursorForward = hCursorForward stdout
cursorBackward = hCursorBackward stdout


hCursorDownLine, hCursorUpLine :: Handle
                               -> Int -- ^ Number of lines to move
                               -> IO ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                             -> IO ()
cursorDownLineCode, cursorUpLineCode :: Int -- ^ Number of lines to move
                                     -> String

cursorDownLine = hCursorDownLine stdout
cursorUpLine = hCursorUpLine stdout


hSetCursorColumn :: Handle
                 -> Int -- ^ 0-based column to move to
                 -> IO ()
setCursorColumn :: Int -- ^ 0-based column to move to
                -> IO ()
setCursorColumnCode :: Int -- ^ 0-based column to move to
                    -> String

setCursorColumn = hSetCursorColumn stdout


hSetCursorPosition :: Handle
                   -> Int -- ^ 0-based row to move to
                   -> Int -- ^ 0-based column to move to
                   -> IO ()
setCursorPosition :: Int -- ^ 0-based row to move to
                  -> Int -- ^ 0-based column to move to
                  -> IO ()
setCursorPositionCode :: Int -- ^ 0-based row to move to
                      -> Int -- ^ 0-based column to move to
                      -> String

setCursorPosition = hSetCursorPosition stdout


hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen :: Handle
                                                                             -> IO ()
clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: IO ()
clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode, clearScreenCode :: String

clearFromCursorToScreenEnd = hClearFromCursorToScreenEnd stdout
clearFromCursorToScreenBeginning = hClearFromCursorToScreenBeginning stdout
clearScreen = hClearScreen stdout


hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine :: Handle
                                                                       -> IO ()
clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: IO ()
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode, clearLineCode :: String

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
-- | Scroll the displayed information up or down the terminal: not widely supported
scrollPageUpCode, scrollPageDownCode :: Int -- ^ Number of lines to scroll by
                                     -> String

scrollPageUp = hScrollPageUp stdout
scrollPageDown = hScrollPageDown stdout


-- | Set the Select Graphic Rendition mode
hSetSGR :: Handle
        -> [SGR] -- ^ Commands: these will typically be applied on top of the current console SGR mode.
                 -- An empty list of commands is equivalent to the list @[Reset]@. Commands are applied
                 -- left to right.
        -> IO ()
-- | Set the Select Graphic Rendition mode
setSGR :: [SGR] -- ^ Commands: these will typically be applied on top of the current console SGR mode.
                -- An empty list of commands is equivalent to the list @[Reset]@. Commands are applied
                -- left to right.
       -> IO ()
-- | Set the Select Graphic Rendition mode
setSGRCode :: [SGR] -- ^ Commands: these will typically be applied on top of the current console SGR mode.
                    -- An empty list of commands is equivalent to the list @[Reset]@. Commands are applied
                    -- left to right.
           -> String

setSGR = hSetSGR stdout


hHideCursor, hShowCursor :: Handle
                         -> IO ()
hideCursor, showCursor :: IO ()
hideCursorCode, showCursorCode :: String

hideCursor = hHideCursor stdout
showCursor = hShowCursor stdout


-- | Set the terminal window title
hSetTitle :: Handle
          -> String -- ^ New title
          -> IO ()
-- | Set the terminal window title
setTitle :: String -- ^ New title
         -> IO ()
-- | Set the terminal window title
setTitleCode :: String -- ^ New title
             -> String

setTitle = hSetTitle stdout