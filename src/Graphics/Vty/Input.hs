{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

-- | The input layer for VTY. This provides methods for initializing an 'Input' structure which can
-- then be used to read 'Event's from the terminal.
--
-- There are two implementations:
--
--  1. Input from terminal devices. Using terminfo to aid event deserialization. Posix systems only.
--
--  2. Windows Console. Windows systems only.
--
-- = Terminfo Based Input
--
-- The history of terminals has resulted in a ugly input process. Some keys and combinations will
-- not reliably map to the expected events by any terminal program. Even those not using vty. There
-- is no 1:1 mapping from key events to bytes read from the terminal input device. In very limited
-- cases the terminal and vty's input process can be customized to resolve these issues.
--
-- See "Graphics.Vty.Config" for how to configure vty's input processing. Customizing terminfo and
-- the terminal is beyond the scope of this documentation.
--
-- == VTY's Implementation
--
-- There are two input modes:
--
--  1. 7 bit
--
--  2. 8 bit
--
-- 7 bit input is the default and the expected in most use cases. This is what vty uses.
--
-- === 7 bit input encoding
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
-- === 8 bit encoding
--
-- The 8th bit was originally used for parity checking. Useless for emulators. Some terminal
-- emulators support a 8 bit input encoding. While this provides some advantages the actual usage is
-- low. Most systems use 7bit mode but recognize 8bit control characters when escaped. This is what
-- vty does.
--
-- === Escaped Control Keys
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
-- === Unicode Input and Escaped Control Key Sequences
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
-- === Terminfo
--
-- The terminfo system is used to determine how some keys are encoded. Terminfo is incomplete. In
-- some cases terminfo is incorrect. Vty assumes terminfo is correct but provides a mechanism to
-- override terminfo. See "Graphics.Vty.Config" specifically 'inputOverrides'.
--
-- === Terminal Input is Broken
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
-- === Terminal Output is Also Broken
--
-- This isn't the only odd aspect of terminals due to historical aspects that no longer apply. EG:
-- Some terminfo capabilities specify millisecond delays. (Capabilities are how terminfo describes
-- the control sequence to output red, for instance) This is to account for the slow speed of
-- hardcopy teletype interfaces. Cause, uh, we totally still use those.
--
-- The output encoding of colors and attributes are also rife with issues.
--
-- = References
--
-- * http://www.leonerd.org.uk/hacks/fixterms/
--
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , Input(..)
                          , inputForConfig -- defined in the platform module
                          )
    where

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Interface

#ifdef POSIX
import Graphics.Vty.Input.Posix
#endif

#ifdef WINDOWS
import Graphics.Vty.Input.Windows
#endif
