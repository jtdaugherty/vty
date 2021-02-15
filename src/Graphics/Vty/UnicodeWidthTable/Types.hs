module Graphics.Vty.UnicodeWidthTable.Types
  ( UnicodeWidthTable(..)
  , WidthTableRange(..)
  )
where

import Data.Word (Word8, Word32)

-- | A range of code points in a width table.
data WidthTableRange =
    WidthTableRange { rangeStart :: Word32
                    -- ^ The range's starting code point.
                    , rangeSize :: Word32
                    -- ^ The number of code points in the contiguous
                    -- range, beginning with the starting code point
                    -- ('rangeStart').
                    , rangeColumns :: Word8
                    -- ^ The terminal width, in columns, of all of the
                    -- characters corresponding to the code points in
                    -- this range.
                    }
    deriving (Eq, Show)

-- | A run-length-encoded table of Unicode character widths.
newtype UnicodeWidthTable =
    UnicodeWidthTable { unicodeWidthTableRanges :: [WidthTableRange]
                      -- ^ The ranges in the table.
                      }
    deriving (Show)
