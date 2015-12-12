{-# LANGUAGE GADTs #-}
module Graphics.Vty.Output.Windows where

import Graphics.Vty.Prelude

import Graphics.Vty.Attributes
import Graphics.Vty.Config
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Output.Interface

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr)
import Data.Bits
import Data.IORef
import Data.Maybe
import Data.Word

import Foreign.C.String
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import System.Win32.Console
import System.Win32.File
import System.Win32.Types

data Win32Color = Win32Color { containsBlue  :: Bool,
                               containsGreen :: Bool,
                               containsRed   :: Bool,
                               intense       :: Bool}
                deriving (Show, Eq)

data Win32Style = Win32Style { fgBold     :: Bool,
                               bgBold     :: Bool,
                               underscore :: Bool }
                deriving (Show, Eq)

data Win32Attr = Win32Attr { fgColor :: Win32Color,
                             bgColor :: Win32Color,
                             style   :: Win32Style }
               deriving (Show, Eq)

toFgColor :: FixedAttr -> Win32Color
toFgColor FixedAttr { fixedForeColor = Just (ISOColor n) } =
    let (n', i) = if n >= 8 then (n - 8, True) else (n, False)
    in isoColor8ToWin32Color n' i
toFgColor _ =
    Win32Color True True True False

toBgColor :: FixedAttr -> Win32Color
toBgColor FixedAttr { fixedBackColor = Just (ISOColor n) } =
    let (n', i) = if n >= 8 then (n - 8, True) else (n, False)
    in isoColor8ToWin32Color n' i
toBgColor _ =
    Win32Color False False False False

isoColor8ToWin32Color :: Word8 -> Bool -> Win32Color
isoColor8ToWin32Color 0 = Win32Color False False False
isoColor8ToWin32Color 1 = Win32Color False False True
isoColor8ToWin32Color 2 = Win32Color False True  False
isoColor8ToWin32Color 3 = Win32Color False True  True
isoColor8ToWin32Color 4 = Win32Color True  False False
isoColor8ToWin32Color 5 = Win32Color True False True
isoColor8ToWin32Color 6 = Win32Color True  True False
isoColor8ToWin32Color 7 = Win32Color True  True  True
isoColor8ToWin32Color n = error $ "ISOColor not in expected range" ++ show n

toStyle :: FixedAttr -> Win32Style
toStyle _fattr = Win32Style False False False

fixedAttrToWin32 :: FixedAttr -> Win32Attr
fixedAttrToWin32 fattr = Win32Attr (toFgColor fattr) (toBgColor fattr) (toStyle fattr)

win32AttrFlags :: Win32Attr -> WORD
win32AttrFlags attr =
    fgColorMask attr .|. bgColorMask attr .|. (textStyleMask $ style attr)

fgColorMask :: Win32Attr -> WORD
fgColorMask attr =
    let color = fgColor attr
    in onBlue fOREGROUND_BLUE color
       .|. onGreen fOREGROUND_GREEN color
       .|. onRed fOREGROUND_RED color
       .|. onIntense fOREGROUND_INTENSITY color

bgColorMask :: Win32Attr -> WORD
bgColorMask attr =
    let color = bgColor attr
    in onBlue bACKGROUND_BLUE color
       .|. onGreen bACKGROUND_GREEN color
       .|. onRed bACKGROUND_RED color
       .|. onIntense bACKGROUND_INTENSITY color

onBlue, onGreen, onRed, onIntense :: WORD -> Win32Color -> WORD
onBlue      mask color = if containsBlue  color then mask else 0
onGreen     mask color = if containsGreen color then mask else 0
onRed       mask color = if containsRed   color then mask else 0
onIntense   mask color = if intense       color then mask else 0

textStyleMask :: Win32Style -> WORD
textStyleMask s =
    let f b m = if b s then m else 0
    in f fgBold fOREGROUND_INTENSITY
       .|. f bgBold bACKGROUND_INTENSITY
       .|. f underscore cOMMON_LVB_UNDERSCORE

-- | Initialize the Win32 Console output support
--
-- 1. Create a file named "CONOUT$". See:
--    https://msdn.microsoft.com/en-us/library/windows/desktop/ms683231
--
-- 2. Set console code page to UTF-8
--    This will enable us to write the UTF-8 bytes to the console handle
--    via WriteFile

outputForConfig :: Config -> IO Output
outputForConfig _config = do
    out <- createFile "CONOUT$"
                      (gENERIC_WRITE .|. gENERIC_READ)
                      (fILE_SHARE_WRITE .|. fILE_SHARE_READ)
                      Nothing
                      oPEN_EXISTING
                      0
                      Nothing
    -- TODO: Constant for this?
    setConsoleCP 65001
    setConsoleOutputCP 65001
    stateRef <- newIORef $ AssumedState Nothing Nothing
    initialAttr <- csbiAttributes <$> currentConsoleScreenBufferInfo out
    return $ Output { terminalID = "win32"
                    , releaseTerminal = liftIO $ closeHandle out
                    , reserveDisplay = return ()
                    , releaseDisplay = return ()
                    , displayBounds = liftIO $ currentConsoleDisplayBounds out
                    , outputDisplayCommands = liftIO . consoleOutputDisplayCommands initialAttr out
                    , contextColorCount = 16
                    , supportsCursorVisibility = True
                    , assumedStateRef = stateRef
                    }

currentConsoleScreenBufferInfo :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFO
currentConsoleScreenBufferInfo out =
    getConsoleScreenBufferInfo out >>=
        maybe (fail "Unable to query console size.")
              return

windowSizeToDisplayRegion :: COORD -> DisplayRegion
windowSizeToDisplayRegion size =
    (fromIntegral $ coordX size, fromIntegral $ coordY size)

currentConsoleDisplayBounds :: HANDLE -> IO DisplayRegion
currentConsoleDisplayBounds out =
    windowSizeToDisplayRegion . csbiMaximumWindowSize <$> currentConsoleScreenBufferInfo out

consoleOutputDisplayCommands :: forall a. WORD -> HANDLE -> DisplayCommands a -> IO a
consoleOutputDisplayCommands initialAttr out = interpret
    where interpret = eval . view
          eval :: ProgramView DisplayCommand a -> IO a
          eval (Return a) = return a
          eval (MoveCursor x y     :>>= cmds) = do
              setConsoleCursorPosition out $ COORD (fromIntegral x) (fromIntegral y)
              interpret $! cmds ()
          eval (ShowCursor         :>>= cmds) = interpret $! cmds ()
          eval (HideCursor         :>>= cmds) = interpret $! cmds ()
          eval (SetAttr prevFAttr newAttr _  :>>= cmds) = do
              let fattr = fixDisplayAttr prevFAttr newAttr
                  attr = fixedAttrToWin32 fattr
                  attrMask = win32AttrFlags attr
              setConsoleTextAttribute out attrMask
              interpret $! cmds ()
          eval (DefaultAttr        :>>= cmds) = do
              setConsoleTextAttribute out initialAttr
              interpret $! cmds ()
          eval (DisplayRowEnd      :>>= cmds) = do
              Just info <- getConsoleScreenBufferInfo out
              let remaining = (coordX $ csbiMaximumWindowSize info) - (coordX $ csbiCursorPosition info)
                  bytes = BS.pack $ replicate (fromIntegral remaining) (toEnum $! fromEnum ' ')
              _ <- BS.useAsCStringLen bytes $ \(ptr, len) ->
                  win32_WriteFile out ptr (fromIntegral len) Nothing
              interpret $! cmds ()
          eval (Utf8Text utf8Bytes :>>= cmds) = do
              _ <- BS.useAsCStringLen utf8Bytes $ \(ptr, len) ->
                  win32_WriteFile out ptr (fromIntegral len) Nothing
              interpret $! cmds ()
