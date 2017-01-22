{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

-- | Display attributes
--
-- Typically the values 'defAttr' or 'currentAttr' are modified to form
-- attributes:
--
-- @
--     defAttr `withForeColor` red
-- @
--
-- Is the attribute that will set the foreground color to red and the
-- background color to the default.
--
-- This can then be used to build an image with a red foreground like
-- so:
--
-- @
--      string (defAttr `withForeColor` red) "this text will be red"
-- @
--
-- The default attributes set by 'defAttr' have a presentation
-- determined by the terminal. This is not something VTY can control.
-- The user is free to define the color scheme of the terminal as they
-- see fit. Up to the limits of the terminal anyways.
--
-- The value 'currentAttr' will keep the attributes of whatever was
-- output previously.
module Graphics.Vty.Attributes
  ( module Graphics.Vty.Attributes
  , module Graphics.Vty.Attributes.Color
  , module Graphics.Vty.Attributes.Color240
  )
where

import Data.Bits
import Data.Default
import Data.Word

import Graphics.Vty.Attributes.Color
import Graphics.Vty.Attributes.Color240

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

-- | A display attribute defines the Color and Style of all the
-- characters rendered after the attribute is applied.
--
-- At most 256 colors, picked from a 240 and 16 color palette, are
-- possible for the background and foreground. The 240 colors and
-- 16 colors are points in different palettes. See Color for more
-- information.
data Attr = Attr
    { attrStyle :: !(MaybeDefault Style)
    , attrForeColor :: !(MaybeDefault Color)
    , attrBackColor :: !(MaybeDefault Color)
    } deriving ( Eq, Show, Read )

-- This could be encoded into a single 32 bit word. The 32 bit word is
-- first divided into 4 groups of 8 bits where: The first group codes
-- what action should be taken with regards to the other groups.
--      XXYYZZ__
--      XX - style action
--          00 => reset to default
--          01 => unchanged
--          10 => set
--      YY - foreground color action
--          00 => reset to default
--          01 => unchanged
--          10 => set
--      ZZ - background color action
--          00 => reset to default
--          01 => unchanged
--          10 => set
--      __ - unused
--
--  Next is the style flags
--      SURBDO__
--      S - standout
--      U - underline
--      R - reverse video
--      B - blink
--      D - dim
--      O - bold
--      __ - unused
--
--  Then the foreground color encoded into 8 bits.
--  Then the background color encoded into 8 bits.

instance Monoid Attr where
    mempty = Attr mempty mempty mempty
    mappend attr0 attr1 =
        Attr ( attrStyle attr0     `mappend` attrStyle attr1 )
             ( attrForeColor attr0 `mappend` attrForeColor attr1 )
             ( attrBackColor attr0 `mappend` attrBackColor attr1 )

-- | Specifies the display attributes such that the final style and
-- color values do not depend on the previously applied display
-- attribute. The display attributes can still depend on the terminal's
-- default colors (unfortunately).
data FixedAttr = FixedAttr
    { fixedStyle :: !Style
    , fixedForeColor :: !(Maybe Color)
    , fixedBackColor :: !(Maybe Color)
    } deriving ( Eq, Show )

-- | The style and color attributes can either be the terminal defaults.
-- Or be equivalent to the previously applied style. Or be a specific
-- value.
data MaybeDefault v where
    Default :: MaybeDefault v
    KeepCurrent :: MaybeDefault v
    SetTo :: forall v . ( Eq v, Show v, Read v ) => !v -> MaybeDefault v

deriving instance Eq v => Eq (MaybeDefault v)
deriving instance Eq v => Show (MaybeDefault v)
deriving instance (Eq v, Show v, Read v) => Read (MaybeDefault v)

instance Eq v => Monoid ( MaybeDefault v ) where
    mempty = KeepCurrent
    mappend Default Default = Default
    mappend Default KeepCurrent = Default
    mappend Default ( SetTo v ) = SetTo v
    mappend KeepCurrent Default = Default
    mappend KeepCurrent KeepCurrent = KeepCurrent
    mappend KeepCurrent ( SetTo v ) = SetTo v
    mappend ( SetTo _v ) Default = Default
    mappend ( SetTo v ) KeepCurrent = SetTo v
    mappend ( SetTo _ ) ( SetTo v ) = SetTo v

-- | Standard 8-color ANSI terminal color codes.
black, red, green, yellow, blue, magenta, cyan, white :: Color
black  = ISOColor 0
red    = ISOColor 1
green  = ISOColor 2
yellow = ISOColor 3
blue   = ISOColor 4
magenta= ISOColor 5
cyan   = ISOColor 6
white  = ISOColor 7

-- | Bright/Vivid variants of the standard 8-color ANSI
brightBlack, brightRed, brightGreen, brightYellow :: Color
brightBlue, brightMagenta, brightCyan, brightWhite :: Color
brightBlack  = ISOColor 8
brightRed    = ISOColor 9
brightGreen  = ISOColor 10
brightYellow = ISOColor 11
brightBlue   = ISOColor 12
brightMagenta= ISOColor 13
brightCyan   = ISOColor 14
brightWhite  = ISOColor 15

-- | Styles are represented as an 8 bit word. Each bit in the word is 1
-- if the style attribute assigned to that bit should be applied and 0
-- if the style attribute should not be applied.
type Style = Word8

-- | The 6 possible style attributes:
--
--      * standout
--
--      * underline
--
--      * reverseVideo
--
--      * blink
--
--      * dim
--
--      * bold/bright
--
--  (The invisible, protect, and altcharset display attributes some
--  terminals support are not supported via VTY.)
standout, underline, reverseVideo, blink, dim, bold :: Style
standout        = 0x01
underline       = 0x02
reverseVideo   = 0x04
blink           = 0x08
dim             = 0x10
bold            = 0x20

defaultStyleMask :: Style
defaultStyleMask = 0x00

styleMask :: Attr -> Word8
styleMask attr
    = case attrStyle attr of
        Default  -> 0
        KeepCurrent -> 0
        SetTo v  -> v

-- | true if the given Style value has the specified Style set.
hasStyle :: Style -> Style -> Bool
hasStyle s bitMask = ( s .&. bitMask ) /= 0

-- | Set the foreground color of an `Attr'.
withForeColor :: Attr -> Color -> Attr
withForeColor attr c = attr { attrForeColor = SetTo c }

-- | Set the background color of an `Attr'.
withBackColor :: Attr -> Color -> Attr
withBackColor attr c = attr { attrBackColor = SetTo c }

-- | Add the given style attribute
withStyle :: Attr -> Style -> Attr
withStyle attr styleFlag = attr { attrStyle = SetTo $ styleMask attr .|. styleFlag }

-- | Sets the style, background color and foreground color to the
-- default values for the terminal. There is no easy way to determine
-- what the default background and foreground colors are.
defAttr :: Attr
defAttr = Attr Default Default Default

instance Default Attr where
    def = defAttr

-- | Keeps the style, background color and foreground color that was
-- previously set. Used to override some part of the previous style.
--
-- EG: current_style `withForeColor` brightMagenta
--
-- Would be the currently applied style (be it underline, bold, etc) but
-- with the foreground color set to brightMagenta.
currentAttr :: Attr
currentAttr = Attr KeepCurrent KeepCurrent KeepCurrent
