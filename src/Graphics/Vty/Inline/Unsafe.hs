{-# OPTIONS_HADDOCK hide #-}
module Graphics.Vty.Inline.Unsafe where

import Graphics.Vty

import Data.Default
import Data.IORef

import System.IO.Unsafe

globalVty :: IORef (Maybe Vty)
{-# NOINLINE globalVty #-}
globalVty = unsafePerformIO $ newIORef Nothing

globalOutput :: IORef (Maybe Output)
{-# NOINLINE globalOutput #-}
globalOutput = unsafePerformIO $ newIORef Nothing

-- | This will create a Vty instance using 'mkVty' and execute an IO action provided that instance.
-- The created Vty instance will be stored to the unsafe 'IORef' 'globalVty'.
withVty :: (Vty -> IO b) -> IO b
withVty f = do
    mvty <- readIORef globalVty
    vty <- case mvty of
        Nothing -> do
            vty <- mkVty $ def
            writeIORef globalVty (Just vty)
            return vty
        Just vty -> return vty
    f vty

withOutput :: (Output -> IO b) -> IO b
withOutput f = do
    mout <- readIORef globalOutput
    out <- case mout of
        Nothing -> do
            config <- userConfig
            out <- outputForCurrentTerminal config
            writeIORef globalOutput (Just out)
            return out
        Just out -> return out
    f out
