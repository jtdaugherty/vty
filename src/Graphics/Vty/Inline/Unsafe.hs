module Graphics.Vty.Inline.Unsafe where

import Graphics.Vty

import Data.Default
import Data.IORef

import System.IO.Unsafe

global_vty :: IORef (Maybe Vty)
{-# NOINLINE global_vty #-}
global_vty = unsafePerformIO $ newIORef Nothing

-- | This will create a Vty instance using 'mkVty' and execute an IO action provided that instance.
-- The created Vty instance will be stored to the unsafe 'IORef' 'global_vty'.
withVty :: (Vty -> IO b) -> IO b
withVty f = do
    mvty <- readIORef global_vty
    vty <- case mvty of
        Nothing -> do
            vty <- mkVty def
            writeIORef global_vty (Just vty)
            return vty
        Just vty -> return vty
    f vty

