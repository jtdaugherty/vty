module Graphics.Vty.Inline.Unsafe where

import Graphics.Vty.Terminal.Interface (Terminal)

import Data.IORef

import System.IO.Unsafe

global_vty_terminal :: IORef (Maybe Terminal)
{-# NOINLINE global_vty_terminal #-}
global_vty_terminal = unsafePerformIO $ newIORef Nothing
