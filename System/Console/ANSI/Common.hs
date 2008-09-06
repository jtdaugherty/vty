module System.Console.ANSI.Common where

-- | ANSI colors: come in various intensities, which are controlled by 'ANSISGR'
data ANSIColor = Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White
               deriving (Bounded, Enum, Show)

-- | ANSI Select Graphic Rendition command
data ANSISGR = Reset
             | BoldIntensity
             | FaintIntensity -- ^ Not widely supported: sometimes treated as conceal
             | NormalIntensity
             | Italic -- ^ Not widely supported: sometimes treated as swapping foreground and background
             | SingleUnderline
             | DoubleUnderline -- ^ Not widely supported
             | NoUnderline
             | SlowBlink
             | RapidBlink
             | NoBlink
             | Conceal -- ^ Not widely supported
             | Reveal
             | SwapForegroundBackground
             | DontSwapForegroundBackground
             | ForegroundNormalIntensity ANSIColor
             | ForegroundHighIntensity ANSIColor
             | BackgroundNormalIntensity ANSIColor
             | BackgroundHighIntensity ANSIColor