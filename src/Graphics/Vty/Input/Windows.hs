module Graphics.Vty.Input.Windows where

import Graphics.Vty.Config
import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Interface

import System.Win32.Console
import System.Win32.File
import System.Win32.Types

inputForConfig :: Config -> IO Input
inputForConfig _ = do
    return undefined
