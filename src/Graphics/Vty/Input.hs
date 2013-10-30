-- Copyright 2009-2010 Corey O'Connor
--
-- There are two input modes:
--  1. 7 bit
--  2. 8 bit
--
-- Which is better? Well, depends on if you want to enter Unicode as UTF-8 or not.
--
-- * 7 bit mode: UTF-8 can be input unambiguiously. Requires an escape delay to differentiate ESC
-- from extended control characters. EG: F1
-- * 8 bit mode: UTF-8 cannot be input. Does not require an escape delay.
--
-- By default 7 bit mode is used. If 8 bit moe is requested then vty will request 8 bit mode from
-- the terminal. However,  if the terminal does not report a capability that uses 8 bit encoding the
-- mode will fall back to 7 bit.
--
-- By default, the escape delay is 10000 microseconds.  Which is assumed to be well above the
-- sampling rate required to detect a keyup for a person typing 200 wpm.
--
-- 0. set raw mode with min == 0 and time == 0
--
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , initTermInput
                          , defaultEscDelay
                          )
    where

import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal
import Graphics.Vty.Input.Terminfo

import Data.Char
import Data.IORef
import Data.Word

import Control.Monad (when, void)
import Control.Concurrent
import Control.Exception

import qualified GHC.Event as GHC

import System.Console.Terminfo

import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Posix.IO ( stdInput
                       , fdReadBuf
                       , setFdOption
                       , FdOption(..)
                       )

import Foreign ( alloca, poke, peek, Ptr )

defaultEscDelay :: Int
defaultEscDelay = 0

-- | Set up the terminal for input.  Returns a function which reads key
-- events, and a function for shutting down the terminal access.
initTermInput :: Int -> Terminal -> IO (IO Event, IO ())
initTermInput escDelay terminal = do
    eventChannel <- newChan
    inputChannel <- newChan
    hadInput <- newEmptyMVar
    attr <- getTerminalAttributes stdInput
    -- disable IXON,
    let attr' = foldl withoutMode attr [ StartStopOutput, KeyboardInterrupts
                                       , EnableEcho, ProcessInput, ExtendedFunctions
                                       ]
    setTerminalAttributes stdInput attr' Immediately
    set_term_timing
    let finishAtomicInput = writeChan inputChannel '\xFFFE'
        inputThread :: IO ()
        inputThread = do
            _ <- alloca $ \(input_buffer :: Ptr Word8) -> do
                let loop = do
                        setFdOption stdInput NonBlockingRead False
                        threadWaitRead stdInput
                        setFdOption stdInput NonBlockingRead True
                        _ <- try readAll :: IO (Either IOException ())
                        when (escDelay == 0) finishAtomicInput
                        loop
                    readAll = do
                        poke input_buffer 0
                        bytes_read <- fdReadBuf stdInput input_buffer 1
                        input_char <- fmap (chr . fromIntegral) $ peek input_buffer
                        when (bytes_read > 0) $ do
                            _ <- tryPutMVar hadInput () -- signal input
                            writeChan inputChannel input_char
                            readAll
                loop
            return ()
        -- | If there is no input for some time, this thread puts '\xFFFE' in the
        -- inputChannel.
        noInputThread :: IO ()
        noInputThread = when (escDelay > 0) loop
            where loop = do
                    takeMVar hadInput -- wait for some input
                    threadDelay escDelay -- microseconds
                    hadNoInput <- isEmptyMVar hadInput -- no input yet?
                    -- TODO(corey): there is a race between here and the inputThread.
                    when hadNoInput finishAtomicInput
                    loop

        caps_legacy_table = map_to_legacy_table $ caps_classify_table terminal keys_from_caps_table
        term_event_classify_table = concat $ caps_legacy_table : ansi_classify_table
        term_event_classifier = classify term_event_classify_table

    eventThreadId <- forkIO $ void $ inputToEventThread term_event_classifier inputChannel eventChannel
    inputThreadId <- forkIO $ inputThread
    noInputThreadId <- forkIO $ noInputThread
    let pokeIO = Catch $ do
            let e = error "(getsize in input layer)"
            setTerminalAttributes stdInput attr' Immediately
            writeChan eventChannel (EvResize e e)
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    -- TODO(corey): killThread is a bit risky for my tastes.
    let uninit = do
            killThread eventThreadId
            killThread inputThreadId
            killThread noInputThreadId
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            setTerminalAttributes stdInput attr Immediately
    return (readChan eventChannel, uninit)

