-- Copyright 2009-2010 Corey O'Connor
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , initTermInput
                          )
    where

import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal

import Data.Char
import Data.Word

import Control.Monad (when)
import Control.Concurrent
import Control.Exception

import System.Console.Terminfo

import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Posix.IO ( stdInput
                       , fdReadBuf
                       , setFdOption
                       , FdOption(..)
                       )

import Foreign ( alloca, poke, peek, Ptr )

-- | Set up the terminal for input.  Returns a function which reads key
-- events, and a function for shutting down the terminal access.
initTermInput :: Int -> Terminal -> IO (IO Event, IO ())
initTermInput escDelay terminal = do
    eventChannel <- newChan
    inputChannel <- newChan
    hadInput <- newEmptyMVar
    oattr <- getTerminalAttributes stdInput
    let nattr = foldl withoutMode oattr [ StartStopOutput, KeyboardInterrupts
                                      , EnableEcho, ProcessInput, ExtendedFunctions
                                      ]
    setTerminalAttributes stdInput nattr Immediately
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
                    when hadNoInput $ do
                        finishAtomicInput
                    loop

        extract_cap = first (getCapability terminal . tiGetStr)
        caps_classify_table = map_to_legacy_table [(x,y) | (Just x,y) <- map extract_cap keys_from_caps_table]
        term_event_classify_table = concat $ caps_classify_table : ansi_classify_table
        term_event_classifier = classify term_event_classify_table

    eventThreadId <- forkIO $ inputToEventThread term_event_classifier inputChannel eventChannel
    inputThreadId <- forkIO $ inputThread
    noInputThreadId <- forkIO $ noInputThread
    let pokeIO = Catch $ do
            let e = error "(getsize in input layer)"
            setTerminalAttributes stdInput nattr Immediately
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
            setTerminalAttributes stdInput oattr Immediately
    return (readChan eventChannel, uninit)

