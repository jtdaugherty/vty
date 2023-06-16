{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Vty.Attributes.Color
  ( Color(..)
  , ColorMode(..)

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

  -- | Bright/Vivid variants of the standard 8-color ANSI
  , brightBlack
  , brightRed
  , brightGreen
  , brightYellow
  , brightBlue
  , brightMagenta
  , brightCyan
  , brightWhite
  -- ** Creating Colors From RGB
  , linearColor
  , srgbColor
  , rgbColor
  , color240
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
-- points are only mapped to the 8 color palette. I'm not sure of
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
data Color = ISOColor !Word8 | Color240 !Word8 | RGBColor !Word8 !Word8 !Word8
    deriving ( Eq, Show, Read, Generic, NFData )

data ColorMode
    = NoColor
    | ColorMode8
    | ColorMode16
    | ColorMode240 !Word8
    | FullColor
    deriving ( Eq, Show )

black, red, green, yellow, blue, magenta, cyan, white :: Color
black  = ISOColor 0
red    = ISOColor 1
green  = ISOColor 2
yellow = ISOColor 3
blue   = ISOColor 4
magenta= ISOColor 5
cyan   = ISOColor 6
white  = ISOColor 7

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

-- | Create a color value from RGB values in the 0..255 range inclusive.
-- No transformation of the input values is done; a color is created
-- directly from the RGB values specified, unlike the 'srgbColor' and
-- 'color240' functions.
linearColor :: Integral i => i -> i -> i -> Color
linearColor r g b = RGBColor r' g' b'
    where
        r' = fromIntegral (clamp r) :: Word8
        g' = fromIntegral (clamp g) :: Word8
        b' = fromIntegral (clamp b) :: Word8
        clamp = min 255 . max 0

-- | Given RGB values in the range 0..255 inclusive, create a color
-- using the sRGB transformation described at
--
-- https://en.wikipedia.org/wiki/SRGB#The_reverse_transformation
srgbColor :: Integral i => i -> i -> i -> Color
srgbColor r g b =
    -- TODO: it may be worth translating this to a lookup table, as with color240
    let shrink n = fromIntegral n / 255 :: Double
        -- called gamma^-1 in wiki
        gamma u
            | u <= 0.04045 = u/12.92
            | otherwise    = ((u + 0.055) / 1.055) ** 2.4
        -- TODO: this is a slightly inaccurate conversion. is it worth doing proterly?
        expand n = round (255 * n)
        convert = expand . gamma . shrink
     in RGBColor (convert r) (convert g) (convert b)

color240 :: Integral i => i -> i -> i -> Color
color240 r g b = Color240 (rgbColorToColor240 r g b)

-- | Create a Vty 'Color' (in the 240 color set) from an RGB triple.
-- This is a synonym for 'color240'. This function is lossy in the sense
-- that we only internally support 240 colors but the #RRGGBB format
-- supports 16^3 colors.
rgbColor :: Integral i => i -> i -> i -> Color
rgbColor = color240
