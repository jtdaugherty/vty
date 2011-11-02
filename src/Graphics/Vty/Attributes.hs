{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- Copyright 2009-2010 Corey O'Connor
-- Display attributes
--
-- For efficiency this can be, in the future, encoded into a single 32 bit word. The 32 bit word is
-- first divided into 4 groups of 8 bits where:
--  The first group codes what action should be taken with regards to the other groups.
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
--
module Graphics.Vty.Attributes
    where

import Data.Bits
import Data.Monoid
import Data.Word

-- | A display attribute defines the Color and Style of all the characters rendered after the
-- attribute is applied.
--
--  At most 256 colors, picked from a 240 and 16 color palette, are possible for the background and
--  foreground. The 240 colors and 16 colors are points in different palettes. See Color for more
--  information.
data Attr = Attr 
    { attr_style :: !(MaybeDefault Style)
    , attr_fore_color :: !(MaybeDefault Color)
    , attr_back_color :: !(MaybeDefault Color)
    } deriving ( Eq, Show )

instance Monoid Attr where
    mempty = Attr mempty mempty mempty
    mappend attr_0 attr_1 = 
        Attr ( attr_style attr_0 `mappend` attr_style attr_1 )
             ( attr_fore_color attr_0 `mappend` attr_fore_color attr_1 )
             ( attr_back_color attr_0 `mappend` attr_back_color attr_1 )

-- | Specifies the display attributes such that the final style and color values do not depend on
-- the previously applied display attribute. The display attributes can still depend on the
-- terminal's default colors (unfortunately).
data FixedAttr = FixedAttr
    { fixed_style :: !Style
    , fixed_fore_color :: !(Maybe Color)
    , fixed_back_color :: !(Maybe Color)
    } deriving ( Eq, Show )

-- | The style and color attributes can either be the terminal defaults. Or be equivalent to the
-- previously applied style. Or be a specific value.
data MaybeDefault v where
    Default :: MaybeDefault v
    KeepCurrent :: MaybeDefault v
    SetTo :: forall v . ( Eq v, Show v ) => !v -> MaybeDefault v

deriving instance Eq v => Eq (MaybeDefault v)
deriving instance Eq v => Show (MaybeDefault v)

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

-- | Abstract data type representing a color.
--  
-- Currently the foreground and background color are specified as points in either a:
--
--  * 16 color palette. Where the first 8 colors are equal to the 8 colors of the ISO 6429 (ANSI) 8
--  color palette and the second 8 colors are bright/vivid versions of the first 8 colors.
--
--  * 240 color palette. This palette is a regular sampling of the full RGB colorspace. 
-- 
-- The 8 ISO 6429 (ANSI) colors are as follows:
--
--      0. black
--
--      1. red
--
--      2. green
--
--      3. yellow
--
--      4. blue
--
--      5. magenta
--
--      6. cyan
--
--      7. white
--
-- The mapping from points in the 240 color palette to colors actually displayable by the terminal
-- depends on the number of colors the terminal claims to support. Which is usually determined by
-- the terminfo "colors" property. If this property is not being accurately reported then the color
-- reproduction will be incorrect.
--
-- If the terminal reports <= 16 colors then the 240 color palette points are only mapped to the 8
-- color pallete. I'm not sure of the RGB points for the "bright" colors which is why they are not
-- addressable via the 240 color palette. 
--
-- If the terminal reports > 16 colors then the 240 color palette points are mapped to the nearest
-- points in a ("color count" - 16) subsampling of the 240 color palette.
--
-- All of this assumes the terminals are behaving similarly to xterm and rxvt when handling colors.
-- And that the individual colors have not been remapped by the user. There may be a way to verify
-- this through terminfo but I don't know it.
--
-- Seriously, terminal color support is INSANE.
data Color = ISOColor !Word8 | Color240 !Word8
    deriving ( Eq, Show )

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
bright_black, bright_red, bright_green, bright_yellow :: Color
bright_blue, bright_magenta, bright_cyan, bright_white :: Color
bright_black  = ISOColor 8
bright_red    = ISOColor 9
bright_green  = ISOColor 10
bright_yellow = ISOColor 11
bright_blue   = ISOColor 12
bright_magenta= ISOColor 13
bright_cyan   = ISOColor 14
bright_white  = ISOColor 15

-- | Styles are represented as an 8 bit word. Each bit in the word is 1 if the style attribute
-- assigned to that bit should be applied and 0 if the style attribute should not be applied.
type Style = Word8

-- | The 6 possible style attributes:
--
--      * standout
--
--      * underline
--
--      * reverse_video
--
--      * blink
--
--      * dim
--
--      * bold/bright
--
--  ( The invisible, protect, and altcharset display attributes some terminals support are not
--  supported via VTY.)
standout, underline, reverse_video, blink, dim, bold :: Style
standout        = 0x01
underline       = 0x02
reverse_video   = 0x04
blink           = 0x08
dim             = 0x10
bold            = 0x20

default_style_mask :: Style
default_style_mask = 0x00

style_mask :: Attr -> Word8
style_mask attr 
    = case attr_style attr of
        Default  -> 0
        KeepCurrent -> 0
        SetTo v  -> v

-- | true if the given Style value has the specified Style set.
has_style :: Style -> Style -> Bool
has_style s bit_mask = ( s .&. bit_mask ) /= 0

-- | Set the foreground color of an `Attr'.
with_fore_color :: Attr -> Color -> Attr
with_fore_color attr c = attr { attr_fore_color = SetTo c }

-- | Set the background color of an `Attr'.
with_back_color :: Attr -> Color -> Attr
with_back_color attr c = attr { attr_back_color = SetTo c }

-- | Add the given style attribute
with_style :: Attr -> Style -> Attr
with_style attr style_flag = attr { attr_style = SetTo $ style_mask attr .|. style_flag }

-- | Sets the style, background color and foreground color to the default values for the terminal.
-- There is no easy way to determine what the default background and foreground colors are.
def_attr :: Attr
def_attr = Attr Default Default Default

-- | Keeps the style, background color and foreground color that was previously set. Used to
-- override some part of the previous style.
--
-- EG: current_style `with_fore_color` bright_magenta
--
-- Would be the currently applied style (be it underline, bold, etc) but with the foreground color
-- set to bright_magenta.
current_attr :: Attr
current_attr = Attr KeepCurrent KeepCurrent KeepCurrent

