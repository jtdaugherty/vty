module Graphics.Vty.Input.Events where

-- | Representations of non-modifier keys.
--
-- * KFun is indexed from 0 to 63. Range of supported FKeys varies by terminal and keyboard.
--
-- * KUpLeft, KUpRight, KDownLeft, KDownRight, KCenter support varies by terminal and keyboard.
--
-- * Actually, support for most of these but KEsc, KChar, KBS, and KEnter vary by terminal and
-- keyboard.
data Key = KEsc  | KChar Char | KBS | KEnter
         | KLeft | KRight | KUp | KDown
         | KUpLeft | KUpRight | KDownLeft | KDownRight | KCenter
         | KFun Int | KBackTab | KPrtScr | KPause | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin | KMenu
    deriving (Eq,Show,Read,Ord)

-- | Modifier keys.  Key codes are interpreted such that users are more likely to
-- have Meta than Alt; for instance on the PC Linux console, 'MMeta' will
-- generally correspond to the physical Alt key.
data Modifier = MShift | MCtrl | MMeta | MAlt
    deriving (Eq,Show,Read,Ord)

-- | Mouse buttons.
--
-- \todo not supported.
data Button = BLeft | BMiddle | BRight
    deriving (Eq,Show,Read,Ord)

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
    | EvResize Int Int
    -- ^ If read from 'eventChannel' this is the size at the time of the
    -- signal. If read from 'nextEvent' this is the size at the time the
    -- event was processed by Vty. Typically these are the same, but if
    -- somebody is resizing the terminal quickly they can be different.
    deriving (Eq,Show,Read,Ord)

type ClassifyMap = [(String,Event)]
