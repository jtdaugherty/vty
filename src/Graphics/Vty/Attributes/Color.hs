module Graphics.Vty.Attributes.Color
  ( Color(..)
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , brightBlack
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
  )
where

import Data.Word

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
    deriving ( Eq, Show, Read )

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
