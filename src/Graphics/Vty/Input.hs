{-# LANGUAGE RecordWildCards #-}

-- | This module provides the input layer for Vty, including methods
-- for initializing an 'Input' structure and reading 'Event's from the
-- terminal.
--
-- Note that due to the evolution of terminal emulators, some keys
-- and combinations will not reliably map to the expected events by
-- any terminal program. There is no 1:1 mapping from key events to
-- bytes read from the terminal input device. In very limited cases the
-- terminal and vty's input process can be customized to resolve these
-- issues; see "Graphics.Vty.Config" for how to configure vty's input
-- processing.
--
-- = VTY's Implementation
--
-- There are two input modes:
--
--  1. 7-bit
--
--  2. 8-bit
--
-- The 7-bit input mode is the default and the expected mode in most use
-- cases. This is what Vty uses.
--
-- == 7-bit input encoding
--
-- Control key combinations are represented by masking the two high bits
-- of the 7-bit input. Historically the control key actually grounded
-- the two high bit wires: 6 and 7. This is why control key combos
-- map to single character events: the input bytes are identical. The
-- input byte is the bit encoding of the character with bits 6 and 7
-- masked. Bit 6 is set by shift. Bit 6 and 7 are masked by control. For
-- example,
--
-- * Control-I is 'i', `01101001`, and has bit 6 and 7 masked to become
-- `00001001`, which is the ASCII and UTF-8 encoding of the Tab key.
--
-- * Control+Shift-C is 'C', `01000011`, with bit 6 and 7 set to zero
-- which is `0000011` and is the "End of Text" code.
--
-- * Hypothesis: This is why capital-A, 'A', has value 65 in ASCII: this
-- is the value 1 with bit 7 set and 6 unset.
--
-- * Hypothesis: Bit 6 is unset by upper case letters because,
-- initially, there were only upper case letters used and a 5 bit
-- encoding.
--
-- == 8-bit encoding
--
-- The 8th bit was originally used for parity checking which is useless
-- for terminal emulators. Some terminal emulators support an 8-bit
-- input encoding. While this provides some advantages, the actual usage
-- is low. Most systems use 7-bit mode but recognize 8-bit control
-- characters when escaped. This is what Vty does.
--
-- == Escaped Control Keys
--
-- Using 7-bit input encoding, the @ESC@ byte can signal the start of
-- an encoded control key. To differentiate a single @ESC@ event from a
-- control key, the timing of the input is used.
--
-- 1. @ESC@ individually: @ESC@ byte; no bytes following for a period of
-- 'VMIN' milliseconds.
--
-- 2. Control keys that contain @ESC@ in their encoding: The @ESC byte
-- is followed by more bytes read within 'VMIN' milliseconds. All bytes
-- up until the next valid input block are passed to the classifier.
--
-- If the current runtime is the threaded runtime then the terminal's
-- @VMIN@ and @VTIME@ behavior reliably implement the above rules. If
-- the current runtime does not support 'forkOS' then there is currently
-- no implementation.
--
-- == Unicode Input and Escaped Control Key Sequences
--
-- The input encoding determines how UTF-8 encoded characters are
-- recognized.
--
-- * 7-bit mode: UTF-8 can be input unambiguiously. UTF-8 input is
-- a superset of ASCII. UTF-8 does not overlap escaped control key
-- sequences. However, the escape key must be differentiated from
-- escaped control key sequences by the timing of the input bytes.
--
-- * 8-bit mode: UTF-8 cannot be input unambiguously. This does not
-- require using the timing of input bytes to differentiate the escape
-- key. Many terminals do not support 8-bit mode.
--
-- == Terminfo
--
-- The terminfo system is used to determine how some keys are encoded.
-- Terminfo is incomplete and in some cases terminfo is incorrect. Vty
-- assumes terminfo is correct but provides a mechanism to override
-- terminfo; see "Graphics.Vty.Config", specifically 'inputOverrides'.
--
-- == Terminal Input is Broken
--
-- Clearly terminal input has fundemental issues. There is no easy way
-- to reliably resolve these issues.
--
-- One resolution would be to ditch standard terminal interfaces
-- entirely and just go directly to scancodes. This would be a
-- reasonable option for Vty if everybody used the linux kernel console
-- but for obvious reasons this is not possible.
--
-- The "Graphics.Vty.Config" module supports customizing the
-- input-byte-to-event mapping and xterm supports customizing the
-- scancode-to-input-byte mapping. With a lot of work a user's system
-- can be set up to encode all the key combos in an almost-sane manner.
--
-- == See also
--
-- * http://www.leonerd.org.uk/hacks/fixterms/
module Graphics.Vty.Input
  ( Key(..)
  , Modifier(..)
  , Button(..)
  , Event(..)
  , Input(..)
  , inputForConfig
  )
where

import Graphics.Vty.Config
import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Loop
import Graphics.Vty.Input.Terminfo

import Control.Concurrent.STM
import Lens.Micro

import qualified System.Console.Terminfo as Terminfo
import System.Posix.Signals.Exts

import Data.Monoid ((<>))

-- | Set up the terminal with file descriptor `inputFd` for input.
-- Returns an 'Input'.
--
-- The table used to determine the 'Events' to produce for the input
-- bytes comes from 'classifyMapForTerm' which is then overridden by
-- the the applicable entries from the configuration's 'inputMap'.
--
-- The terminal device is configured with the attributes:
--
-- * IXON disabled: disables software flow control on outgoing data.
-- This stops the process from being suspended if the output terminal
-- cannot keep up.
--
-- * Raw mode is used for input.
--
-- * ISIG disabled (enables keyboard combinations that result in
-- signals)
--
-- * ECHO disabled (input is not echoed to the output)
--
-- * ICANON disabled (canonical mode (line mode) input is not used)
--
-- * IEXTEN disabled (extended functions are disabled)
inputForConfig :: Config -> IO Input
inputForConfig config@Config{ termName = Just termName
                            , inputFd = Just termFd
                            , vmin = Just _
                            , vtime = Just _
                            , .. } = do
    terminal <- Terminfo.setupTerm termName
    let inputOverrides = [(s,e) | (t,s,e) <- inputMap, t == Nothing || t == Just termName]
        activeInputMap = classifyMapForTerm termName terminal `mappend` inputOverrides
    (setAttrs,unsetAttrs) <- attributeControl termFd
    setAttrs
    input <- initInput config activeInputMap
    let pokeIO = Catch $ do
            let e = error "vty internal failure: this value should not propagate to users"
            setAttrs
            atomically $ writeTChan (input^.eventChannel) (EvResize e e)
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    return $ input
        { shutdownInput = do
            shutdownInput input
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            unsetAttrs
        }
inputForConfig config = (<> config) <$> standardIOConfig >>= inputForConfig
