{-# LANGUAGE RecordWildCards #-}
-- | The input layer for VTY. This provides methods for initializing an 'Input' structure which can
-- then be used to read 'Event's from the terminal.
--
-- The history of terminals has resulted in a broken input process. Some keys and combinations will
-- not reliably map to the expected events by any terminal program. Even those not using vty. There
-- is no 1:1 mapping from key events to bytes read from the terminal input device. In very limited
-- cases the terminal and vty's input process can be customized to resolve these issues.
--
-- See "Graphics.Vty.Config" for how to configure vty's input processing. Customizing terminfo and
-- the terminal is beyond the scope of this documentation.
--
-- = VTY's Implementation
--
-- One can get the brain rot trying to understand all this. So, as far as I can care...
--
-- There are two input modes:
--
--  1. 7 bit
--
--  2. 8 bit
--
-- 7 bit input is the default and the expected in most use cases. This is what vty uses.
--
-- == 7 bit input encoding
--
-- Control key combinations are represented by masking the two high bits of the 7bit input.  Back in
-- the day the control key actually grounded the two high bit wires: 6 and 7. This is why
-- control key combos map to single character events: The input bytes are identical. The input byte
-- is the bit encoding of the character with bits 6 and 7 masked.  Bit 6 is set by shift. Bit 6 and
-- 7 are masked by control. EG:
--
-- * Control-I is 'i', `01101001`, has bit 6 and 7 masked to become `00001001`. Which is the ASCII
-- and UTF-8 encoding of the tab key.
--
-- * Control+Shift-C is 'C', `01000011`, with bit 6 and 7 set to zero which makes `0000011` and
-- is the "End of Text" code.
--
-- * Hypothesis: This is why capital-A, 'A', has value 65 in ASCII: This is the value 1 with bit 7
-- set and 6 unset.
--
-- * Hypothesis: Bit 6 is unset by upper case letters because, initially, there were only upper case
-- letters used and a 5 bit encoding.
--
-- == 8 bit encoding
--
-- The 8th bit was originally used for parity checking. Useless for emulators. Some terminal
-- emulators support a 8 bit input encoding. While this provides some advantages the actual usage is
-- low. Most systems use 7bit mode but recognize 8bit control characters when escaped. This is what
-- vty does.
--
-- == Escaped Control Keys
--
-- Using 7 bit input encoding the @ESC@ byte can signal the start of an encoded control key. To
-- differentiate a single @ESC@ eventfrom a control key the timing of the input is used.
--
-- 1. @ESC@ individually: @ESC@ byte; no bytes for 'singleEscPeriod'.
--
-- 2. control keys that contain @ESC@ in their encoding: The @ESC byte; followed by more bytes read
-- within 'singleEscPeriod'. All bytes up until the next valid input block are passed to the
-- classifier.
--
-- If the current runtime is the threaded runtime then the terminal's @VMIN@ and @VTIME@ behavior
-- reliably implement the above rules.  If the current runtime does not support forkOS then there is
-- currently no implementation.
--
-- Vty used to emulate @VMIN@ and @VTIME@. This was a input loop which did tricky things with
-- non-blocking reads and timers. The implementation was not reliable. A reliable implementation is
-- possible, but there are no plans to implement this.
--
-- == Unicode Input and Escaped Control Key Sequences
--
-- The input encoding determines how UTF-8 encoded characters are recognize.
--
-- * 7 bit mode: UTF-8 can be input unambiguiously. UTF-8 input is a superset of ASCII. UTF-8 does
-- not overlap escaped control key sequences. However, the escape key must be differentiated from
-- escaped control key sequences by the timing of the input bytes.
--
-- * 8 bit mode: UTF-8 cannot be input unambiguously. This does not require using the timing of
-- input bytes to differentiate the escape key. Many terminals do not support 8 bit mode.
--
-- == Terminfo
--
-- The terminfo system is used to determine how some keys are encoded. Terminfo is incomplete. In
-- some cases terminfo is incorrect. Vty assumes terminfo is correct but provides a mechanism to
-- override terminfo. See "Graphics.Vty.Config" specifically 'inputOverrides'.
--
-- == Terminal Input is Broken
--
-- Clearly terminal input has fundemental issues. There is no easy way to reliably resolve these
-- issues.
--
-- One resolution would be to ditch standard terminal interfaces entirely and just go directly to
-- scancodes. A reasonable option for vty if everybody used the linux kernel console. I hear GUIs
-- are popular these days. Sadly, GUI terminal emulators don't provide access to scancodes AFAIK.
--
-- All is lost? Not really. "Graphics.Vty.Config" supports customizing the input byte to event
-- mapping and xterm supports customizing the scancode to input byte mapping. With a lot of work a
-- user's system can be set up to encode all the key combos in an almost-sane manner.
--
-- There are other tricky work arounds that can be done. I have no interest in implementing most of
-- these. They are not really worth the time.
--
-- == Terminal Output is Also Broken
--
-- This isn't the only odd aspect of terminals due to historical aspects that no longer apply. EG:
-- Some terminfo capabilities specify millisecond delays. (Capabilities are how terminfo describes
-- the control sequence to output red, for instance) This is to account for the slow speed of
-- hardcopy teletype interfaces. Cause, uh, we totally still use those.
-- 
-- The output encoding of colors and attributes are also rife with issues.
--
-- == See also
--
-- * http://www.leonerd.org.uk/hacks/fixterms/
--
-- In my experience this cannot resolve the issues without changes to the terminal emulator and
-- device.
module Graphics.Vty.Input ( Key(..)
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

import Control.Concurrent
import Control.Lens

import Data.Functor ((<$>))
import Data.Monoid

import qualified System.Console.Terminfo as Terminfo
import System.Posix.Signals.Exts

-- | Set up the terminal with file descriptor `inputFd` for input.  Returns a 'Input'.
--
-- The table used to determine the 'Events' to produce for the input bytes comes from
-- 'classifyMapForTerm'. Which is then overridden by the the applicable entries from
-- 'inputMap'.
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
--      - extended functions are disabled. TODO: I don't know what those are.
--
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
inputForConfig config = mappend config <$> standardIOConfig >>= inputForConfig
