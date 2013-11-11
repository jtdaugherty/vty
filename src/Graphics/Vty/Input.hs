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

import Control.Concurrent

import System.Console.Terminfo

import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Posix.IO (stdInput)

defaultEscDelay :: Int
defaultEscDelay = 4000

-- | Set up the terminal for input.  Returns a function which reads key
-- events, and a function for shutting down the terminal access.
initTermInput :: Int -> Terminal -> IO (IO Event, IO ())
initTermInput escDelay terminal = do
    attr <- getTerminalAttributes stdInput
    -- disable IXON,
    let attr' = foldl withoutMode attr [ StartStopOutput, KeyboardInterrupts
                                       , EnableEcho, ProcessInput, ExtendedFunctions
                                       ]
    setTerminalAttributes stdInput attr' Immediately
    set_term_timing
    let caps_legacy_table = map_to_legacy_table $ caps_classify_table terminal keys_from_caps_table
        classify_table    = concat $ caps_legacy_table : ansi_classify_table
    (eventChannel, shutdown_input) <- initInputForFd escDelay classify_table stdInput
    let pokeIO = Catch $ do
            let e = error "(getsize in input layer)"
            setTerminalAttributes stdInput attr' Immediately
            writeChan eventChannel (EvResize e e)
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    let shutdown_input' = do
            shutdown_input
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            setTerminalAttributes stdInput attr Immediately
    return (readChan eventChannel, shutdown_input')

