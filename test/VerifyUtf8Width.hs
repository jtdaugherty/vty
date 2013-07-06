module VerifyUtf8Width where
import Verify

import Graphics.Text.Width
import Graphics.Vty.Attributes
import Graphics.Vty.Picture

sw_is_1_column :: SingleColumnChar -> Bool
sw_is_1_column (SingleColumnChar c) = image_width (char def_attr c) == 1

dw_is_2_column :: DoubleColumnChar -> Bool
dw_is_2_column (DoubleColumnChar c) = image_width (char def_attr c) == 2

dc_string_is_even :: NonEmptyList DoubleColumnChar -> Bool
dc_string_is_even (NonEmpty dw_list) =
    even $ safe_wcswidth [ c | DoubleColumnChar c <- dw_list ]

safe_wcwidth_for_control_chars :: Bool
safe_wcwidth_for_control_chars = 0 == safe_wcwidth '\NUL'

tests :: IO [Test]
tests = return
  [ verify "sw_is_1_column" sw_is_1_column
  , verify "dw_is_2_column" dw_is_2_column
  , verify "a string of double characters is an even width" dc_string_is_even
  , verify "safe_wcwidth provides a width of 0 for chars without widths" safe_wcwidth_for_control_chars
  ]

