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
-- * 8 bit mode: UTF-8 cannot be input unambiguously AFAIK. Does not require an escape delay.
--
-- vty assumes 7 bit mode. Which is the default AFAIK.
--
-- 1. ESC individually: ESC byte, no bytes for 'singleEscPeriod'.
--
-- 2. control keys that contain ESC in their byte sequence: ESC byte. Bytes read within
-- 'singleEscPeriod' are part of the sequence up until the next valid input block.
--
-- If the current runtime is the threaded runtime then the terminal's VMIN and VTIME parameters can
-- be applied to implement the above rules.
-- If the current runtime does not support forkOS then there is currently no implementation. Vty
-- used to implement a parser which did tricky things with non-blocking reads and timers. This could
-- be revived.
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , Input(..)
                          , Config(..)
                          , inputForCurrentTerminal
                          , inputForNameAndIO
                          )
    where

import Graphics.Vty.Config
import Graphics.Vty.Input.Classify
import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Loop
import Graphics.Vty.Input.Terminfo

import Control.Applicative
import Control.Concurrent
import Control.Lens

import Data.Monoid

import qualified System.Console.Terminfo as Terminfo
import System.Environment
import System.Posix.Signals.Exts
import System.Posix.IO (stdInput)
import System.Posix.Types (Fd)

-- | Set up the current terminal for input.
-- This determines the current terminal then invokes 'inputForNameAndIO'
inputForCurrentTerminal :: Config -> IO Input
inputForCurrentTerminal config = do
    termName <- getEnv "TERM"
    inputForNameAndIO config termName stdInput

-- | Set up the terminal attached to the given Fd for input.  Returns a 'Input'.
--
-- The table used to determine the 'Events' to produce for the input bytes comes from
-- 'classifyTableForTerm'. Which is then overridden by the table from 'classifyTableUserOverrides'.
--
-- The terminal device is configured with the attributes:
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
--      - enables keyboard combinations that result in signals. TODO: should probably be a dynamic
--      option.
--
-- * ECHO disabled
--      - input is not echod to the output. TODO: should be a dynamic option.
--
-- * ICANON disabled
--      - canonical mode (line mode) input is not used. TODO: should be a dynamic option.
--
-- * IEXTEN disabled
--      - extended functions are disabled. TODO: Uh. Whatever these are.
--
inputForNameAndIO :: Config -> String -> Fd -> IO Input
inputForNameAndIO config termName termFd = do
    terminal <- Terminfo.setupTerm termName
    classifyTable <- mappend <$> pure (classifyTableForTerm termName terminal)
                             <*> classifyTableUserOverrides
    (setAttrs,unsetAttrs) <- attributeControl termFd
    setAttrs
    input <- initInputForFd config classifyTable termFd 
    let pokeIO = Catch $ do
            let e = error "vty internal failure: this value should not propagate to users"
            setAttrs
            writeChan (input^.eventChannel) (EvResize e e)
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    return $ input
        { shutdownInput = do
            shutdownInput input
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            unsetAttrs
        }
