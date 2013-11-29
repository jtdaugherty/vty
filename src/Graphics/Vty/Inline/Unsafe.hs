module Graphics.Vty.Inline.Unsafe where

import Graphics.Vty

import Data.IORef

import System.IO.Unsafe

global_vty :: IORef (Maybe Vty)
{-# NOINLINE global_vty #-}
global_vty = unsafePerformIO $ newIORef Nothing

withVty :: (Vty -> IO b) -> IO b
withVty f = do
    mvty <- readIORef global_vty
    vty <- case mvty of
        Nothing -> do
            vty <- mkVty
            writeIORef global_vty (Just vty)
            return vty
        Just vty -> return vty
    f vty

