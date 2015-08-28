{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

module Graphics.Vty.Inline.Unsafe where

import Graphics.Vty

#ifdef POSIX
import Graphics.Vty.Inline.Unsafe.Posix
#endif

import Data.Default
import Data.Monoid
import Data.IORef

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif

import System.IO.Unsafe

globalVty :: IORef (Maybe Vty)
{-# NOINLINE globalVty #-}
globalVty = unsafePerformIO $ newIORef Nothing

globalOutput :: IORef (Maybe Output)
{-# NOINLINE globalOutput #-}
globalOutput = unsafePerformIO $ newIORef Nothing

-- | This will create a Vty instance using 'mkVty' and execute an IO action provided that instance.
-- The created Vty instance will be stored to the unsafe 'IORef' 'globalVty'.
--
-- This instance will use duplicates of the stdin and stdout Handles.
withVty :: (Vty -> IO b) -> IO b
withVty f = do
    mvty <- readIORef globalVty
    vty <- case mvty of
        Nothing -> do
#ifdef POSIX
            vty <- mkDupeConfig >>= mkVty
#endif
#ifdef WINDOWS
            vty <- mkVty def
#endif
            writeIORef globalVty (Just vty)
            return vty
        Just vty -> return vty
    f vty

withOutput :: (Output -> IO b) -> IO b
withOutput f = do
    mout <- readIORef globalOutput
    out <- case mout of
        Nothing -> do
#ifdef POSIX
            config <- mappend <$> userConfig <*> mkDupeConfig
#endif
#ifdef WINDOWS
            config <- userConfig
#endif
            out <- outputForConfig config
            writeIORef globalOutput (Just out)
            return out
        Just out -> return out
    f out
