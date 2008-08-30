module System.Console.ANSI.Common where

data ANSIColor = Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White
               deriving (Bounded, Enum, Show)

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