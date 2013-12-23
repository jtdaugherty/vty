-- | The input layer for VTY. This provides methods for initializing an 'Input' structure which can
-- then be used to read 'Event's from the terminal.
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , Input(..)
                          , defaultEscDelay
                          , input_for_current_terminal
                          , input_for_name_and_io
                          )
    where

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Internal
import Graphics.Vty.Input.Terminfo

import Control.Concurrent

import qualified System.Console.Terminfo as Terminfo
import System.Environment
import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import System.Posix.Types (Fd)

-- Right, I'm mostly guessing on these details. So, as far as I can figure:
--
-- There are two input modes:
--  1. 7 bit
--  2. 8 bit
--
-- Which is better? Well, depends on if you want to enter Unicode as UTF-8 or not.
--
-- * 7 bit mode: UTF-8 can be input unambiguiously.
-- * 8 bit mode: UTF-8 cannot be input. Does not require an escape delay.
--
-- vty uses 7 bit mode.
--
-- To differentiate between ESC and control keys that contain ESC:
-- vty using the timing of the input to differentiate.
-- This, I best I can figure, is what VMIN and VTIME are supposed to be for.
-- For some reason, I don't yet know, vty implements the timing in the library.
-- I'm not sure if this is an advantage or not!

-- | By default, the escape delay is 10000 microseconds.  Which is assumed to be well above the
-- sampling rate required to detect a keyup for a person typing 200 wpm.
defaultEscDelay :: Int
defaultEscDelay = 10000

-- | Set up the current terminal for input.
-- This determines the current terminal then invokes 'input_for_name_and_io'
input_for_current_terminal :: Int -> IO Input
input_for_current_terminal escDelay = do
    term_name <- getEnv "TERM"
    input_for_name_and_io escDelay term_name stdInput

-- | Set up the terminal attached to the given Fd for input.  Returns a 'Input'.
--
-- The terminal is modified as follows:
--
-- * IXON disabled
--      - disables software flow control on outgoing data. This stops the process from being
--        suspended if the output terminal cannot keep up. I presume this has little effect these
--        days. I hope this means that output will be buffered if the terminal cannot keep up. In the
--        old days the output might of been dropped.
-- 
-- "raw" mode is used for input.
--
-- * ISIG disabled
--      - enables keyboard combinations that result in signals. This should probably be a dynamic
--      option.
--
-- * ECHO disabled
--      - input is not echod to the output. Pobably should be a dynamic option.
--
-- * ICANON disabled
--      - canonical mode (line mode) input is not used. Probably should be a dynamic option.
--
-- * IEXTEN disabled
--      - extended functions are disabled. Uh. Whatever these are.
--
input_for_name_and_io :: Int -> String -> Fd -> IO Input
input_for_name_and_io escDelay term_name term_in = do
    terminal <- Terminfo.setupTerm term_name
    attr <- getTerminalAttributes term_in
    let attr' = foldl withoutMode attr [ StartStopOutput, KeyboardInterrupts
                                       , EnableEcho, ProcessInput, ExtendedFunctions
                                       ]
    setTerminalAttributes term_in attr' Immediately
    set_term_timing term_in
    let classify_table = classify_table_for_term terminal
    (eventChannel, shutdown_event_processing) <- initInputForFd escDelay classify_table term_in
    let pokeIO = Catch $ do
            let e = error "(getsize in input layer)"
            setTerminalAttributes term_in attr' Immediately
            writeChan eventChannel (EvResize e e)
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    return $ Input
        { event_channel  = eventChannel
        , shutdown_input = do
            shutdown_event_processing
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            setTerminalAttributes term_in attr Immediately
        }
