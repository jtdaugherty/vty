module Main where
import Graphics.Vty.Attributes
import Graphics.Vty.Image

import Verify

two_sw_horiz_concat :: SingleColumnChar -> SingleColumnChar -> Bool
two_sw_horiz_concat (SingleColumnChar c1) (SingleColumnChar c2) = 
    image_width (char def_attr c1 <|> char def_attr c2) == 2

many_sw_horiz_concat :: [SingleColumnChar] -> Bool
many_sw_horiz_concat cs = 
    let chars = [ char | SingleColumnChar char <- cs ]
        l = fromIntegral $ length cs
    in image_width ( horiz_cat $ map (char def_attr) chars ) == l

two_sw_vert_concat :: SingleColumnChar -> SingleColumnChar -> Bool
two_sw_vert_concat (SingleColumnChar c1) (SingleColumnChar c2) = 
    image_height (char def_attr c1 <-> char def_attr c2) == 2

horiz_concat_sw_assoc :: SingleColumnChar -> SingleColumnChar -> SingleColumnChar -> Bool
horiz_concat_sw_assoc (SingleColumnChar c0) (SingleColumnChar c1) (SingleColumnChar c2) = 
    (char def_attr c0 <|> char def_attr c1) <|> char def_attr c2 
    == 
    char def_attr c0 <|> (char def_attr c1 <|> char def_attr c2)

two_dw_horiz_concat :: DoubleColumnChar -> DoubleColumnChar -> Bool
two_dw_horiz_concat (DoubleColumnChar c1) (DoubleColumnChar c2) = 
    image_width (char def_attr c1 <|> char def_attr c2) == 4

many_dw_horiz_concat :: [DoubleColumnChar] -> Bool
many_dw_horiz_concat cs = 
    let chars = [ char | DoubleColumnChar char <- cs ]
        l = fromIntegral $ length cs
    in image_width ( horiz_cat $ map (char def_attr) chars ) == l * 2

two_dw_vert_concat :: DoubleColumnChar -> DoubleColumnChar -> Bool
two_dw_vert_concat (DoubleColumnChar c1) (DoubleColumnChar c2) = 
    image_height (char def_attr c1 <-> char def_attr c2) == 2

horiz_concat_dw_assoc :: DoubleColumnChar -> DoubleColumnChar -> DoubleColumnChar -> Bool
horiz_concat_dw_assoc (DoubleColumnChar c0) (DoubleColumnChar c1) (DoubleColumnChar c2) = 
    (char def_attr c0 <|> char def_attr c1) <|> char def_attr c2 
    == 
    char def_attr c0 <|> (char def_attr c1 <|> char def_attr c2)

main = run_test $ do
    verify "two_sw_horiz_concat" two_sw_horiz_concat
    verify "many_sw_horiz_concat" many_sw_horiz_concat
    verify "two_sw_vert_concat" two_sw_vert_concat
    verify "horiz_concat_sw_assoc" horiz_concat_sw_assoc
    verify "many_dw_horiz_concat" many_dw_horiz_concat
    verify "two_dw_horiz_concat" two_dw_horiz_concat
    verify "two_dw_vert_concat" two_dw_vert_concat
    verify "horiz_concat_dw_assoc" horiz_concat_dw_assoc
    return ()

