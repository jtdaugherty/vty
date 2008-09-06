-- * Basic data types
module System.Console.ANSI.Common,

-- * Cursor movement by character
cursorUp, cursorDown, cursorForward, cursorBackward,
hCursorUp, hCursorDown, hCursorForward, hCursorBackward,

-- * Cursor movement by line
nextLine, previousLine,
hNextLine, hPreviousLine,

-- * Directly changing cursor position
setColumn,
hSetColumn,

setPosition,
hSetPosition,

-- * Clearing parts of the screen
clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen,
hClearFromCursorToScreenEnd, hClearFromCursorToScreenBeginning, hClearScreen,

clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine,
hClearFromCursorToLineEnd, hClearFromCursorToLineBeginning, hClearLine,

-- * Scrolling the screen
scrollPageUp, scrollPageDown,
hScrollPageUp, hScrollPageDown,

-- * Select Graphic Rendition mode: colors and other whizzy stuff
setSGR,
hSetSGR,

-- * Cursor visibilty changes
hideCursor, showCursor,
hHideCursor, hShowCursor