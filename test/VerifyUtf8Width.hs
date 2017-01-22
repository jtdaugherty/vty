module VerifyUtf8Width where

import Verify

import Graphics.Text.Width
import Graphics.Vty.Attributes
import Graphics.Vty.Picture

swIs1Column :: SingleColumnChar -> Bool
swIs1Column (SingleColumnChar c) = imageWidth (char defAttr c) == 1

dwIs2Column :: DoubleColumnChar -> Bool
dwIs2Column (DoubleColumnChar c) = imageWidth (char defAttr c) == 2

dcStringIsEven :: NonEmptyList DoubleColumnChar -> Bool
dcStringIsEven (NonEmpty dw_list) =
    even $ safeWcswidth [ c | DoubleColumnChar c <- dw_list ]

safeWcwidthForControlChars :: Bool
safeWcwidthForControlChars = 0 == safeWcwidth '\NUL'

tests :: IO [Test]
tests = return
  [ verify "swIs1Column" swIs1Column
  , verify "dwIs2Column" dwIs2Column
  , verify "a string of double characters is an even width" dcStringIsEven
  , verify "safeWcwidth provides a width of 0 for chars without widths" safeWcwidthForControlChars
  ]

