{-# LANGUAGE DeriveAnyClass, DeriveGeneric, PatternSynonyms #-}

module Graphics.Vty.Attributes.Color
  ( Color(..)

  -- ** Fixed Colors
  -- | Standard 8-color ANSI terminal color codes.
  --
  -- Note that these map to colors in the terminal's custom palette. For
  -- instance, `white` maps to whatever the terminal color theme uses for
  -- white.
  --
  -- Use these functions if you want to make apps that fit the terminal theme.
  -- If you want access to more/stronger colors use `rgbColor`
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , pattern Black
  , pattern Red
  , pattern Green
  , pattern Yellow
  , pattern Blue
  , pattern Magenta
  , pattern Cyan
  , pattern White

  -- | Bright/Vivid variants of the standard 8-color ANSI
  , brightBlack
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
  , pattern BrightBlack
  , pattern BrightRed
  , pattern BrightGreen
  , pattern BrightYellow
  , pattern BrightBlue
  , pattern BrightMagenta
  , pattern BrightCyan
  , pattern BrightWhite
  -- ** Creating Colors From RGB
  , rgbColor
  , module Graphics.Vty.Attributes.Color240
  )
where

import Data.Word
import GHC.Generics
import Control.DeepSeq

import Graphics.Vty.Attributes.Color240

-- | Abstract data type representing a color.
--
-- Currently the foreground and background color are specified as points
-- in either a:
--
--  * 16 color palette. Where the first 8 colors are equal to the 8
--  colors of the ISO 6429 (ANSI) 8 color palette and the second 8
--  colors are bright/vivid versions of the first 8 colors.
--
--  * 240 color palette. This palette is a regular sampling of the full
--  RGB colorspace for the first 224 colors. The remaining 16 colors is
--  a greyscale palette.
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
-- The mapping from points in the 240 color palette to colors actually
-- displayable by the terminal depends on the number of colors the
-- terminal claims to support. Which is usually determined by the
-- terminfo "colors" property. If this property is not being accurately
-- reported then the color reproduction will be incorrect.
--
-- If the terminal reports <= 16 colors then the 240 color palette
-- points are only mapped to the 8 color pallete. I'm not sure of
-- the RGB points for the "bright" colors which is why they are not
-- addressable via the 240 color palette.
--
-- If the terminal reports > 16 colors then the 240 color palette
-- points are mapped to the nearest points in a ("color count" - 16)
-- subsampling of the 240 color palette.
--
-- All of this assumes the terminals are behaving similarly to xterm and
-- rxvt when handling colors. And that the individual colors have not
-- been remapped by the user. There may be a way to verify this through
-- terminfo but I don't know it.
--
-- Seriously, terminal color support is INSANE.
data Color = ISOColor !Word8 | Color240 !Word8
    deriving ( Eq, Show, Read, Generic, NFData )

pattern Black, Red, Green, Yellow, Blue, Magenta, Cyan, White :: Color
pattern Black  = ISOColor 0
pattern Red    = ISOColor 1
pattern Green  = ISOColor 2
pattern Yellow = ISOColor 3
pattern Blue   = ISOColor 4
pattern Magenta= ISOColor 5
pattern Cyan   = ISOColor 6
pattern White  = ISOColor 7

{-# DEPRECATED black, red, green, yellow, blue, magenta, cyan, white "Use the pattern synonyms instead" #-}
black, red, green, yellow, blue, magenta, cyan, white :: Color
black   = Black
red     = Red
green   = Green
yellow  = Yellow
blue    = Blue
magenta = Magenta
cyan    = Cyan
white   = White


pattern BrightBlack, BrightRed, BrightGreen, BrightYellow :: Color
pattern BrightBlue, BrightMagenta, BrightCyan, BrightWhite :: Color
pattern BrightBlack  = ISOColor 8
pattern BrightRed    = ISOColor 9
pattern BrightGreen  = ISOColor 10
pattern BrightYellow = ISOColor 11
pattern BrightBlue   = ISOColor 12
pattern BrightMagenta= ISOColor 13
pattern BrightCyan   = ISOColor 14
pattern BrightWhite  = ISOColor 15

{-# DEPRECATED brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite "Use the pattern synonyms instead" #-}
brightBlack, brightRed, brightGreen, brightYellow :: Color
brightBlue, brightMagenta, brightCyan, brightWhite :: Color
brightBlack  = BrightBlack
brightRed    = BrightRed
brightGreen  = BrightGreen
brightYellow = BrightYellow
brightBlue   = BrightBlue
brightMagenta= BrightMagenta
brightCyan   = BrightCyan
brightWhite  = BrightWhite

-- | Create a Vty 'Color' (in the 240 color set) from an RGB triple.
-- This function is lossy in the sense that we only internally support 240 colors but the
-- #RRGGBB format supports 16^3 colors.
rgbColor :: Integral i => i -> i -> i -> Color
rgbColor r g b = Color240 (rgbColorToColor240 r g b)
