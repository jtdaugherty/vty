module System.Console.ANSI.Common where

data ANSIColor = Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White

data ANSISGR = Reset
             | BoldIntensity
             | FaintIntensity -- ^ Not widely supported
             | NormalIntensity
             | Italic -- ^ Not widely supported: sometimes treated as swapping foreground and background
             | SingleUnderline -- ^ Not widely supported
             | DoubleUnderline
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