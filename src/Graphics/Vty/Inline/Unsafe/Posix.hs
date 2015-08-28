module Graphics.Vty.Inline.Unsafe.Posix where

import Data.Default

import GHC.IO.Handle (hDuplicate)

import System.IO (stdin, stdout, hSetBuffering, BufferMode(NoBuffering))

import System.Posix.IO (handleToFd)

mkDupeConfig :: IO Config
mkDupeConfig = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    stdinDupe <- hDuplicate stdin >>= handleToFd
    stdoutDupe <- hDuplicate stdout >>= handleToFd
    return $ def { inputFd = Just stdinDupe, outputFd = Just stdoutDupe }
