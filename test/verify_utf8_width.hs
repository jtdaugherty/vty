module Main where
import Verify

import Graphics.Vty.Attributes
import Graphics.Vty.Picture

sw_is_1_column :: SingleColumnChar -> Bool
sw_is_1_column (SingleColumnChar c) = image_width (char def_attr c) == 1

dw_is_2_column :: DoubleColumnChar -> Bool
dw_is_2_column (DoubleColumnChar c) = image_width (char def_attr c) == 2

main = run_test $ do
    verify "sw_is_1_column" sw_is_1_column
    verify "dw_is_2_column" dw_is_2_column
    return ()

