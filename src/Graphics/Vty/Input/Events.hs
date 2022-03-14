{-# Language DeriveGeneric #-}
{-# Language StrictData #-}
module Graphics.Vty.Input.Events where

import Control.DeepSeq
import Data.ByteString
import GHC.Generics

-- | Representations of non-modifier keys.
--
-- * KFun is indexed from 0 to 63. Range of supported FKeys varies by
-- terminal and keyboard.
--
-- * KUpLeft, KUpRight, KDownLeft, KDownRight, KCenter support varies by
-- terminal and keyboard.
--
-- * Actually, support for most of these but KEsc, KChar, KBS, and
-- KEnter vary by terminal and keyboard.
data Key = KEsc  | KChar {-# UNPACK #-} Char | KBS | KEnter
         | KLeft | KRight | KUp | KDown
         | KUpLeft | KUpRight | KDownLeft | KDownRight | KCenter
         | KFun {-# UNPACK #-} Int | KBackTab | KPrtScr | KPause | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin | KMenu
    deriving (Eq,Show,Read,Ord,Generic)

instance NFData Key

-- | Modifier keys. Key codes are interpreted such that users are more
-- likely to have Meta than Alt; for instance on the PC Linux console,
-- 'MMeta' will generally correspond to the physical Alt key.
data Modifier = MShift | MCtrl | MMeta | MAlt
    deriving (Eq,Show,Read,Ord,Generic)

instance NFData Modifier

-- | Mouse buttons.
data Button = BLeft | BMiddle | BRight | BScrollUp | BScrollDown
    deriving (Eq,Show,Read,Ord,Generic)

instance NFData Button

-- | Events.
data Event
    = EvKey Key [Modifier]
    -- ^ A keyboard key was pressed with the specified modifiers.
    | EvMouseDown Int Int Button [Modifier]
    -- ^ A mouse button was pressed at the specified column and row. Any
    -- modifiers available in the event are also provided.
    | EvMouseUp Int Int (Maybe Button)
    -- ^ A mouse button was released at the specified column and
    -- row. Some terminals report only that a button was released
    -- without specifying which one; in that case, Nothing is provided.
    -- Otherwise Just the button released is included in the event.
    | EvResize ~Int ~Int
    -- ^ If read from 'eventChannel' this is the size at the time of the
    -- signal. If read from 'nextEvent' this is the size at the time the
    -- event was processed by Vty. Typically these are the same, but if
    -- somebody is resizing the terminal quickly they can be different.
    --
    -- Note: this is a lazy constructor to support bottom/error values
    -- passing through the input events channel.
    | EvPaste ByteString
    -- ^ A paste event occurs when a bracketed paste input sequence is
    -- received. For terminals that support bracketed paste mode, these
    -- events will be triggered on a paste event. Terminals that do not
    -- support bracketed pastes will send the paste contents as ordinary
    -- input (which is probably bad, so beware!) Note that the data is
    -- provided in raw form and you'll have to decode (e.g. as UTF-8) if
    -- that's what your application expects.
    | EvLostFocus
    -- ^ The terminal running the application lost input focus.
    | EvGainedFocus
    -- ^ The terminal running the application gained input focus.
    deriving (Eq,Show,Read,Ord,Generic)

instance NFData Event

type ClassifyMap = [(String,Event)]
