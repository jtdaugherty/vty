-- | The input layer for VTY. This provides methods for initializing an 'Input' structure which can
-- then be used to read 'Event's from the terminal.
--
-- Right, brain rot trying to figure this out exactly. So, as far as I can care:
--
-- There are two input modes for control sequences:
--
--  1. 7 bit
--
--  2. 8 bit
--
-- Which should be used depends on the need to enter Unicode as UTF-8 or not.
--
-- * 7 bit mode: UTF-8 can be input unambiguiously. The timing of ESC in the input sequence
-- determines whether the ESC is: Individual, part of a control key sequence, part of a meta key
-- combination.
--
-- * 8 bit mode: UTF-8 cannot be input unamboguiously AFAIK. Does not require an escape delay.
--
-- vty assumes 7 bit mode. Which is the default AFAIK.
--
-- 1. ESC individually: ESC byte, no bytes for control-seq-period,
-- no bytes for meta-combo-period.
--
-- 2. control keys that contain ESC in their byte sequence: ESC byte, bytes read within
-- control-seq-period are part of the sequence. Up until the next valid input block.
--
-- 3. ESC used as meta in a key combination: ESC byte, no other bytes read within
-- control-seq-period. Bytes up until next valid input block are the input events plus meta.
--
-- This, as best I can figure, is what VMIN and VTIME are supposed to be for.  For some reason vty
-- implements the timing in the library.  I'm not sure if this is an advantage or not. Getting this
-- right has provided a strangely interesting task so I haven't cared to determine.
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , Input(..)
                          , Config(..)
                          , input_for_current_terminal
                          , input_for_name_and_io
                          )
    where

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Internal
import Graphics.Vty.Input.Terminfo

import Control.Concurrent

import Data.IORef

import qualified System.Console.Terminfo as Terminfo
import System.Environment
import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Posix.IO (stdInput)
import System.Posix.Types (Fd)

-- | Set up the current terminal for input.
-- This determines the current terminal then invokes 'input_for_name_and_io'
input_for_current_terminal :: Config -> IO Input
input_for_current_terminal config = do
    term_name <- getEnv "TERM"
    input_for_name_and_io config term_name stdInput

-- | Set up the terminal attached to the given Fd for input.  Returns a 'Input'.
--
-- The terminal is modified as follows:
--
-- * IXON disabled
--      - disables software flow control on outgoing data. This stops the process from being
--        suspended if the output terminal cannot keep up. I presume this has little effect these
--        days. I hope this means that output will be buffered if the terminal cannot keep up. In the
--        old days the output might of been dropped?
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
input_for_name_and_io :: Config -> String -> Fd -> IO Input
input_for_name_and_io config term_name term_fd = do
    terminal <- Terminfo.setupTerm term_name
    attr <- getTerminalAttributes term_fd
    let attr' = foldl withoutMode attr [ StartStopOutput, KeyboardInterrupts
                                       , EnableEcho, ProcessInput, ExtendedFunctions
                                       ]
    setTerminalAttributes term_fd attr' Immediately
    let classify_table = classify_table_for_term term_name terminal
    apply_timing_config term_fd config
    input <- newIORef config >>= \ref -> initInputForFd ref classify_table term_fd 
    let pokeIO = Catch $ do
            let e = error "(getsize in input layer)"
            setTerminalAttributes term_fd attr' Immediately
            writeChan (_event_channel input) (EvResize e e)
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    return $ input
        { shutdown_input = do
            shutdown_input input
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            setTerminalAttributes term_fd attr Immediately
        }
