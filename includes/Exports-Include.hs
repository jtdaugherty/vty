-- * Basic data types
module System.Console.ANSI.Common,

-- * Cursor movement by character
cursorUp, cursorDown, cursorForward, cursorBackward,
hCursorUp, hCursorDown, hCursorForward, hCursorBackward,
cursorUpCode, cursorDownCode, cursorForwardCode, cursorBackwardCode,

-- * Cursor movement by line
nextLine, previousLine,
hNextLine, hPreviousLine,
nextLineCode, previousLineCode,

-- * Directly changing cursor position
setColumn,
hSetColumn,
setColumnCode,

setPosition,
hSetPosition,
setPositionCode,

-- * Clearing parts of the screen
clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen,
hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen,
clearFromCursorToScreenEndCode, clearFromCursorToScreenBeginningCode, clearScreenCode,

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine,
hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine,
clearFromCursorToLineEndCode, clearFromCursorToLineBeginningCode, clearLineCode,

-- * Scrolling the screen
scrollPageUp, scrollPageDown,
hScrollPageUp, hScrollPageDown,
scrollPageUpCode, scrollPageDownCode,

-- * Select Graphic Rendition mode: colors and other whizzy stuff
setSGR,
hSetSGR,
setSGRCode,

-- * Cursor visibilty changes
hideCursor, showCursor,
hHideCursor, hShowCursor,
hideCursorCode, showCursorCode