{-# LANGUAGE ScopedTypeVariables #-}
module VerifyImageOps where

import Graphics.Vty.Attributes
import Graphics.Vty.Image.Internal
import Verify.Graphics.Vty.Image

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

vert_contat_single_row :: NonEmptyList SingleRowSingleAttrImage -> Bool
vert_contat_single_row (NonEmpty stack) =
    let expected_height :: Int = length stack
        stack_image = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- stack ]
    in image_height stack_image == expected_height

disjoint_height_horiz_join :: NonEmptyList SingleRowSingleAttrImage 
                              -> NonEmptyList SingleRowSingleAttrImage
                              -> Bool
disjoint_height_horiz_join (NonEmpty stack_0) (NonEmpty stack_1) =
    let expected_height :: Int = max (length stack_0) (length stack_1)
        stack_image_0 = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- stack_0 ]
        stack_image_1 = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- stack_1 ]
    in image_height (stack_image_0 <|> stack_image_1) == expected_height


disjoint_height_horiz_join_bg_fill :: NonEmptyList SingleRowSingleAttrImage 
                                      -> NonEmptyList SingleRowSingleAttrImage
                                      -> Bool
disjoint_height_horiz_join_bg_fill (NonEmpty stack_0) (NonEmpty stack_1) =
    let stack_image_0 = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- stack_0 ]
        stack_image_1 = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- stack_1 ]
        image = stack_image_0 <|> stack_image_1
        expected_height = image_height image
    in case image of
        HorizJoin {}  -> ( expected_height == (image_height $ part_left image) )
                         && 
                         ( expected_height == (image_height $ part_right image) )
        _             -> True

disjoint_width_vert_join :: NonEmptyList SingleRowSingleAttrImage 
                            -> NonEmptyList SingleRowSingleAttrImage
                            -> Bool
disjoint_width_vert_join (NonEmpty stack_0) (NonEmpty stack_1) =
    let expected_width = maximum $ map image_width (stack_0_images ++ stack_1_images)
        stack_0_images = [ i | SingleRowSingleAttrImage { row_image = i } <- stack_0 ]
        stack_1_images = [ i | SingleRowSingleAttrImage { row_image = i } <- stack_1 ]
        stack_0_image = vert_cat stack_0_images 
        stack_1_image = vert_cat stack_1_images 
        image = stack_0_image <-> stack_1_image
    in image_width image == expected_width

disjoint_width_vert_join_bg_fill :: NonEmptyList SingleRowSingleAttrImage 
                            -> NonEmptyList SingleRowSingleAttrImage
                            -> Bool
disjoint_width_vert_join_bg_fill (NonEmpty stack_0) (NonEmpty stack_1) =
    let expected_width = maximum $ map image_width (stack_0_images ++ stack_1_images)
        stack_0_images = [ i | SingleRowSingleAttrImage { row_image = i } <- stack_0 ]
        stack_1_images = [ i | SingleRowSingleAttrImage { row_image = i } <- stack_1 ]
        stack_0_image = vert_cat stack_0_images 
        stack_1_image = vert_cat stack_1_images 
        image = stack_0_image <-> stack_1_image
    in case image of
        VertJoin {} -> ( expected_width == (image_width $ part_top image) )
                       &&
                       ( expected_width == (image_width $ part_bottom image) )
        _           -> True

translation_is_linear_on_out_size :: Translation -> Bool
translation_is_linear_on_out_size (Translation i (x,y) i') =
    image_width i' == image_width i + x && image_height i' == image_height i + y

padding_is_linear_on_out_size :: Image -> Gen Bool
padding_is_linear_on_out_size i = do
    l <- offset
    t <- offset
    r <- offset
    b <- offset
    let i' = pad l t r b i
    return $ image_width i' == image_width i + l + r && image_height i' == image_height i + t + b
    where offset = choose (1,1024)

crop_left_limits_width :: Image -> Int -> Property
crop_left_limits_width i v = v >= 0 ==>
    v >= image_width (crop_left v i)

crop_right_limits_width :: Image -> Int -> Property
crop_right_limits_width i v = v >= 0 ==>
    v >= image_width (crop_right v i)

crop_top_limits_height :: Image -> Int -> Property
crop_top_limits_height i v = v >= 0 ==>
    v >= image_height (crop_top v i)

crop_bottom_limits_height :: Image -> Int -> Property
crop_bottom_limits_height i v = v >= 0 ==>
    v >= image_height (crop_bottom v i)

tests :: IO [Test]
tests = return
    [ verify "two_sw_horiz_concat" two_sw_horiz_concat
    , verify "many_sw_horiz_concat" many_sw_horiz_concat
    , verify "two_sw_vert_concat" two_sw_vert_concat
    , verify "horiz_concat_sw_assoc" horiz_concat_sw_assoc
    , verify "many_dw_horiz_concat" many_dw_horiz_concat
    , verify "two_dw_horiz_concat" two_dw_horiz_concat
    , verify "two_dw_vert_concat" two_dw_vert_concat
    , verify "horiz_concat_dw_assoc" horiz_concat_dw_assoc
    , verify "single row vert concats to correct height" vert_contat_single_row
    , verify "disjoint_height_horiz_join" disjoint_height_horiz_join
    , verify "disjoint_height_horiz_join BG fill" disjoint_height_horiz_join_bg_fill
    , verify "disjoint_width_vert_join" disjoint_width_vert_join
    , verify "disjoint_width_vert_join BG fill" disjoint_width_vert_join_bg_fill
    , verify "translation effects output dimensions linearly" translation_is_linear_on_out_size
    , verify "padding effects output dimensions linearly" padding_is_linear_on_out_size
    , verify "crop left limits width" crop_left_limits_width
    , verify "crop right limits width" crop_right_limits_width
    , verify "crop top limits height" crop_top_limits_height
    , verify "crop bottom limits height" crop_bottom_limits_height
    ]

