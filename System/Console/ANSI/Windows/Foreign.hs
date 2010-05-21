-- | "System.Win32.Console" is really very impoverished, so I have had to do all the FFI myself.
module System.Console.ANSI.Windows.Foreign (
        -- Re-exports from Win32.Types
        BOOL, WORD, DWORD, WCHAR, HANDLE, SHORT,
        
        charToWCHAR,
        
        COORD(..), SMALL_RECT(..), rect_top, rect_bottom, rect_left, rect_right, rect_width, rect_height,
        CONSOLE_CURSOR_INFO(..), CONSOLE_SCREEN_BUFFER_INFO(..), CHAR_INFO(..),
        
        sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE, sTD_ERROR_HANDLE,
        
        fOREGROUND_BLUE, fOREGROUND_GREEN, fOREGROUND_RED, fOREGROUND_INTENSITY, fOREGROUND_WHITE, fOREGROUND_INTENSE_WHITE,
        bACKGROUND_BLUE, bACKGROUND_GREEN, bACKGROUND_RED, bACKGROUND_INTENSITY, bACKGROUND_WHITE, bACKGROUND_INTENSE_WHITE,
        cOMMON_LVB_REVERSE_VIDEO, cOMMON_LVB_UNDERSCORE,
        
        getStdHandle,
        getConsoleScreenBufferInfo,
        getConsoleCursorInfo,
        
        setConsoleTextAttribute,
        setConsoleCursorPosition,
        setConsoleCursorInfo,
        setConsoleTitle,
        
        fillConsoleOutputAttribute,
        fillConsoleOutputCharacter,
        scrollConsoleScreenBuffer,
        
        withTString, withHandleToHANDLE
    ) where

import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Data.Bits
import Data.Char

import System.Win32.Types

import Control.Concurrent.MVar
import Control.Exception (bracket)

import Foreign.StablePtr

import GHC.IOBase (Handle(..), Handle__(..))
import qualified GHC.IOBase as IOBase (FD) -- Just an Int32

#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.FD (FD(..)) -- A wrapper around an Int32
import Data.Typeable
#endif


-- Some Windows types missing from System.Win32
type SHORT = CShort
type WCHAR = CWchar

charToWCHAR :: Char -> WCHAR
charToWCHAR char = fromIntegral (ord char)


-- This is a FFI hack. Some of the API calls take a Coord, but that isn't a built-in FFI type so I can't
-- use it directly. Instead, I use UNPACKED_COORD and marshal COORDs into this manually. Note that we CAN'T
-- just use two SHORTs directly because they get expanded to 4 bytes each instead of just boing 2 lots of 2
-- bytes by the stdcall convention, so linking fails.
type UNPACKED_COORD = CInt

-- Field packing order determined experimentally: I couldn't immediately find a specification for Windows
-- struct layout anywhere.
unpackCOORD :: COORD -> UNPACKED_COORD
unpackCOORD (COORD x y) = (fromIntegral y) `shiftL` (sizeOf x * 8) .|. (fromIntegral x)


peekAndOffset :: Storable a => Ptr a -> IO (a, Ptr b)
peekAndOffset ptr = do
    item <- peek ptr
    return (item, ptr `plusPtr` sizeOf item)

pokeAndOffset :: Storable a => Ptr a -> a -> IO (Ptr b)
pokeAndOffset ptr item = do
    poke ptr item
    return (ptr `plusPtr` sizeOf item)


data COORD = COORD {
        coord_x :: SHORT,
        coord_y :: SHORT
    }

instance Show COORD where
    show (COORD x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Storable COORD where
    sizeOf ~(COORD x y) = sizeOf x + sizeOf y
    alignment ~(COORD x _) = alignment x
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr SHORT
        x <- peekElemOff ptr' 0
        y <- peekElemOff ptr' 1
        return (COORD x y)
    poke ptr (COORD x y) = do
        let ptr' = castPtr ptr :: Ptr SHORT
        pokeElemOff ptr' 0 x
        pokeElemOff ptr' 1 y


data SMALL_RECT = SMALL_RECT {
        rect_top_left :: COORD,
        rect_bottom_right :: COORD
    }

rect_top, rect_left, rect_bottom, rect_right :: SMALL_RECT -> SHORT
rect_top = coord_y . rect_top_left
rect_left = coord_x . rect_top_left
rect_bottom = coord_y . rect_bottom_right
rect_right = coord_x . rect_bottom_right

rect_width, rect_height :: SMALL_RECT -> SHORT
rect_width rect = rect_right rect - rect_left rect + 1
rect_height rect = rect_bottom rect - rect_top rect + 1

instance Show SMALL_RECT where
    show (SMALL_RECT tl br) = show tl ++ "-" ++ show br

instance Storable SMALL_RECT where
    sizeOf ~(SMALL_RECT tl br) = sizeOf tl + sizeOf br
    alignment ~(SMALL_RECT tl _) = alignment tl
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr COORD
        tl <- peekElemOff ptr' 0
        br <- peekElemOff ptr' 1
        return (SMALL_RECT tl br)
    poke ptr (SMALL_RECT tl br) = do
        let ptr' = castPtr ptr :: Ptr COORD
        pokeElemOff ptr' 0 tl
        pokeElemOff ptr' 1 br


data CONSOLE_CURSOR_INFO = CONSOLE_CURSOR_INFO {
        cci_cursor_size :: DWORD,
        cci_cursor_visible :: BOOL
    }
    deriving (Show)

instance Storable CONSOLE_CURSOR_INFO where
    sizeOf ~(CONSOLE_CURSOR_INFO size visible) = sizeOf size + sizeOf visible
    alignment ~(CONSOLE_CURSOR_INFO size _) = alignment size
    peek ptr = do
        (size, ptr') <- peekAndOffset (castPtr ptr)
        visible <- peek ptr'
        return (CONSOLE_CURSOR_INFO size visible)
    poke ptr (CONSOLE_CURSOR_INFO size visible) = do
        ptr' <- pokeAndOffset (castPtr ptr) size
        poke ptr' visible


data CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO {
        csbi_size :: COORD,
        csbi_cursor_position :: COORD,
        csbi_attributes :: WORD,
        csbi_window :: SMALL_RECT,
        csbi_maximum_window_size :: COORD
    }
    deriving (Show)

instance Storable CONSOLE_SCREEN_BUFFER_INFO where
    sizeOf ~(CONSOLE_SCREEN_BUFFER_INFO size cursor_position attributes window maximum_window_size)
      = sizeOf size + sizeOf cursor_position + sizeOf attributes + sizeOf window + sizeOf maximum_window_size
    alignment ~(CONSOLE_SCREEN_BUFFER_INFO size _ _ _ _) = alignment size
    peek ptr = do
        (size, ptr1) <- peekAndOffset (castPtr ptr)
        (cursor_position, ptr2) <- peekAndOffset ptr1
        (attributes, ptr3) <- peekAndOffset ptr2
        (window, ptr4) <- peekAndOffset ptr3
        maximum_window_size <- peek ptr4
        return (CONSOLE_SCREEN_BUFFER_INFO size cursor_position attributes window maximum_window_size)
    poke ptr (CONSOLE_SCREEN_BUFFER_INFO size cursor_position attributes window maximum_window_size) = do
        ptr1 <- pokeAndOffset (castPtr ptr) size
        ptr2 <- pokeAndOffset ptr1 cursor_position
        ptr3 <- pokeAndOffset ptr2 attributes
        ptr4 <- pokeAndOffset ptr3 window
        poke ptr4 maximum_window_size


data CHAR_INFO = CHAR_INFO {
        ci_char :: WCHAR,
        ci_attributes :: WORD
    }
    deriving (Show)

instance Storable CHAR_INFO where
    sizeOf ~(CHAR_INFO char attributes) = sizeOf char + sizeOf attributes
    alignment ~(CHAR_INFO char _) = alignment char
    peek ptr = do
        (char, ptr') <- peekAndOffset (castPtr ptr)
        attributes <- peek ptr'
        return (CHAR_INFO char attributes)
    poke ptr (CHAR_INFO char attributes) = do
        ptr' <- pokeAndOffset (castPtr ptr) char
        poke ptr' attributes


sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE, sTD_ERROR_HANDLE :: DWORD
sTD_INPUT_HANDLE = -10
sTD_OUTPUT_HANDLE = -11
sTD_ERROR_HANDLE = -12

fOREGROUND_BLUE, fOREGROUND_GREEN, fOREGROUND_RED, fOREGROUND_INTENSITY,
  bACKGROUND_BLUE, bACKGROUND_GREEN, bACKGROUND_RED, bACKGROUND_INTENSITY,
  cOMMON_LVB_REVERSE_VIDEO, cOMMON_LVB_UNDERSCORE :: WORD
fOREGROUND_BLUE = 0x1
fOREGROUND_GREEN = 0x2
fOREGROUND_RED = 0x4
fOREGROUND_INTENSITY = 0x8
bACKGROUND_BLUE = 0x10
bACKGROUND_GREEN = 0x20
bACKGROUND_RED= 0x40
bACKGROUND_INTENSITY = 0x80
cOMMON_LVB_REVERSE_VIDEO = 0x4000
cOMMON_LVB_UNDERSCORE = 0x8000

fOREGROUND_WHITE, bACKGROUND_WHITE, fOREGROUND_INTENSE_WHITE, bACKGROUND_INTENSE_WHITE :: WORD
fOREGROUND_WHITE = fOREGROUND_RED .|. fOREGROUND_GREEN .|. fOREGROUND_BLUE
bACKGROUND_WHITE = bACKGROUND_RED .|. bACKGROUND_GREEN .|. bACKGROUND_BLUE
fOREGROUND_INTENSE_WHITE = fOREGROUND_WHITE .|. fOREGROUND_INTENSITY
bACKGROUND_INTENSE_WHITE = bACKGROUND_WHITE .|. bACKGROUND_INTENSITY


foreign import stdcall unsafe "windows.h GetStdHandle" getStdHandle :: DWORD -> IO HANDLE
foreign import stdcall unsafe "windows.h GetConsoleScreenBufferInfo" cGetConsoleScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO BOOL
foreign import stdcall unsafe "windows.h GetConsoleCursorInfo" cGetConsoleCursorInfo :: HANDLE -> Ptr CONSOLE_CURSOR_INFO -> IO BOOL

foreign import stdcall unsafe "windows.h SetConsoleTextAttribute" cSetConsoleTextAttribute :: HANDLE -> WORD -> IO BOOL
foreign import stdcall unsafe "windows.h SetConsoleCursorPosition" cSetConsoleCursorPosition :: HANDLE -> UNPACKED_COORD -> IO BOOL
foreign import stdcall unsafe "windows.h SetConsoleCursorInfo" cSetConsoleCursorInfo :: HANDLE -> Ptr CONSOLE_CURSOR_INFO -> IO BOOL
foreign import stdcall unsafe "windows.h SetConsoleTitleW" cSetConsoleTitle :: LPCTSTR -> IO BOOL

foreign import stdcall unsafe "windows.h FillConsoleOutputAttribute" cFillConsoleOutputAttribute :: HANDLE -> WORD -> DWORD -> UNPACKED_COORD -> Ptr DWORD -> IO BOOL
foreign import stdcall unsafe "windows.h FillConsoleOutputCharacterW" cFillConsoleOutputCharacter :: HANDLE -> TCHAR -> DWORD -> UNPACKED_COORD -> Ptr DWORD -> IO BOOL
foreign import stdcall unsafe "windows.h ScrollConsoleScreenBufferW" cScrollConsoleScreenBuffer :: HANDLE -> Ptr SMALL_RECT -> Ptr SMALL_RECT -> UNPACKED_COORD -> Ptr CHAR_INFO -> IO BOOL


getConsoleScreenBufferInfo :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFO
getConsoleScreenBufferInfo handle = alloca $ \ptr_console_screen_buffer_info -> do
    failIfFalse_ "getConsoleScreenBufferInfo" $ cGetConsoleScreenBufferInfo handle ptr_console_screen_buffer_info
    peek ptr_console_screen_buffer_info

getConsoleCursorInfo :: HANDLE -> IO CONSOLE_CURSOR_INFO
getConsoleCursorInfo handle = alloca $ \ptr_console_cursor_info -> do
    failIfFalse_ "getConsoleCursorInfo" $ cGetConsoleCursorInfo handle ptr_console_cursor_info
    peek ptr_console_cursor_info


setConsoleTextAttribute :: HANDLE -> WORD -> IO ()
setConsoleTextAttribute handle attributes = failIfFalse_ "setConsoleTextAttribute" $ cSetConsoleTextAttribute handle attributes

setConsoleCursorPosition :: HANDLE -> COORD -> IO ()
setConsoleCursorPosition handle cursor_position = failIfFalse_ "setConsoleCursorPosition" $ cSetConsoleCursorPosition handle (unpackCOORD cursor_position)

setConsoleCursorInfo :: HANDLE -> CONSOLE_CURSOR_INFO -> IO ()
setConsoleCursorInfo handle console_cursor_info = with console_cursor_info $ \ptr_console_cursor_info -> do
    failIfFalse_ "setConsoleCursorInfo" $ cSetConsoleCursorInfo handle ptr_console_cursor_info

setConsoleTitle :: LPCTSTR -> IO ()
setConsoleTitle title = failIfFalse_ "setConsoleTitle" $ cSetConsoleTitle title


fillConsoleOutputAttribute :: HANDLE -> WORD -> DWORD -> COORD -> IO DWORD
fillConsoleOutputAttribute handle attribute fill_length write_origin = alloca $ \ptr_chars_written -> do
    failIfFalse_ "fillConsoleOutputAttribute" $ cFillConsoleOutputAttribute handle attribute fill_length (unpackCOORD write_origin) ptr_chars_written
    peek ptr_chars_written

fillConsoleOutputCharacter :: HANDLE -> TCHAR -> DWORD -> COORD -> IO DWORD
fillConsoleOutputCharacter handle char fill_length write_origin = alloca $ \ptr_chars_written -> do
    failIfFalse_ "fillConsoleOutputCharacter" $ cFillConsoleOutputCharacter handle char fill_length (unpackCOORD write_origin) ptr_chars_written
    peek ptr_chars_written

scrollConsoleScreenBuffer :: HANDLE -> SMALL_RECT -> Maybe SMALL_RECT -> COORD -> CHAR_INFO -> IO ()
scrollConsoleScreenBuffer handle scroll_rectangle mb_clip_rectangle destination_origin fill 
  = with scroll_rectangle $ \ptr_scroll_rectangle ->
    maybeWith with mb_clip_rectangle $ \ptr_clip_rectangle ->
    with fill $ \ptr_fill ->
    failIfFalse_ "scrollConsoleScreenBuffer" $ cScrollConsoleScreenBuffer handle ptr_scroll_rectangle ptr_clip_rectangle (unpackCOORD destination_origin) ptr_fill


-- This essential function comes from msvcrt.  It's OK to depend on msvcrt since GHC's base package does.
foreign import ccall unsafe "_get_osfhandle" cget_osfhandle :: IOBase.FD -> IO HANDLE

-- | This bit is all highly dubious.  The problem is that we want to output ANSI to arbitrary Handles rather than forcing
-- people to use stdout.  However, the Windows ANSI emulator needs a Windows HANDLE to work it's magic, so we need to be able
-- to extract one of those from the Haskell Handle.
--
-- This code accomplishes this, albeit at the cost of only being compatible with GHC.
withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLE haskell_handle action = 
    -- Create a stable pointer to the Handle. This prevents the garbage collector
    -- getting to it while we are doing horrible manipulations with it, and hence
    -- stops it being finalized (and closed).
    withStablePtr haskell_handle $ const $ do
        -- Grab the write handle variable from the Handle
        let write_handle_mvar = case haskell_handle of
                FileHandle _ handle_mvar     -> handle_mvar
                DuplexHandle _ _ handle_mvar -> handle_mvar -- This is "write" MVar, we could also take the "read" one
        
        -- Get the FD from the algebraic data type
#if __GLASGOW_HASKELL__ < 612
        fd <- fmap haFD $ readMVar write_handle_mvar
#else
        --readMVar write_handle_mvar >>= \(Handle__ { haDevice = dev }) -> print (typeOf dev)
        Just fd <- fmap (\(Handle__ { haDevice = dev }) -> fmap fdFD (cast dev)) $ readMVar write_handle_mvar
#endif

        -- Finally, turn that (C-land) FD into a HANDLE using msvcrt
        windows_handle <- cget_osfhandle fd
        
        -- Do what the user originally wanted
        action windows_handle

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr