{-# LANGUAGE GADTs #-}
module Graphics.Vty.Output.Windows where

import Graphics.Vty.Prelude

import Graphics.Vty.Config
import Graphics.Vty.Output.Interface

import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr)
import Data.Bits
import Data.IORef
import Data.Maybe

import Foreign.C.String
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr
import System.Win32.Console
import System.Win32.File
import System.Win32.Types

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
    return $ Output { terminalID = "win32"
                    , releaseTerminal = liftIO $ closeHandle out
                    , reserveDisplay = return ()
                    , releaseDisplay = return ()
                    , displayBounds = liftIO $ consoleDisplayBounds out
                    , outputDisplayCommands = liftIO . consoleOutputDisplayCommands out
                    , contextColorCount = 16
                    , supportsCursorVisibility = True
                    , assumedStateRef = stateRef
                    }

consoleDisplayBounds :: HANDLE -> IO DisplayRegion
consoleDisplayBounds out =
    getConsoleScreenBufferInfo out >>=
        maybe (fail "Unable to query console size.")
              (\info -> let size = csbiMaximumWindowSize info
                        in return (fromIntegral $ coordX size, fromIntegral $ coordY size)
              )

consoleOutputDisplayCommands :: forall a. HANDLE -> DisplayCommands a -> IO a
consoleOutputDisplayCommands out = interpret
    where interpret = eval . view
          eval :: ProgramView DisplayCommand a -> IO a
          eval (Return a) = return a
          eval (MoveCursor x y     :>>= cmds) = do
              setConsoleCursorPosition out $ COORD (fromIntegral x) (fromIntegral y)
              interpret $! cmds ()
          eval (ShowCursor         :>>= cmds) = interpret $! cmds ()
          eval (HideCursor         :>>= cmds) = interpret $! cmds ()
          eval (SetAttr fattr _ _  :>>= cmds) = interpret $! cmds ()
          eval (DefaultAttr        :>>= cmds) = interpret $! cmds ()
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
