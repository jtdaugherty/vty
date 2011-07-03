{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Graphics.Vty.Attributes
import Verify.Graphics.Vty.Image

import Verify

import Data.Word

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
    let expected_height :: Word = fromIntegral $ length stack
        stack_image = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- stack ]
    in image_height stack_image == expected_height

disjoint_height_horiz_join :: NonEmptyList SingleRowSingleAttrImage 
                              -> NonEmptyList SingleRowSingleAttrImage
                              -> Bool
disjoint_height_horiz_join (NonEmpty stack_0) (NonEmpty stack_1) =
    let expected_height :: Word = fromIntegral $ max (length stack_0) (length stack_1)
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

main = run_test $ do
    _ <- verify "two_sw_horiz_concat" two_sw_horiz_concat
    _ <- verify "many_sw_horiz_concat" many_sw_horiz_concat
    _ <- verify "two_sw_vert_concat" two_sw_vert_concat
    _ <- verify "horiz_concat_sw_assoc" horiz_concat_sw_assoc
    _ <- verify "many_dw_horiz_concat" many_dw_horiz_concat
    _ <- verify "two_dw_horiz_concat" two_dw_horiz_concat
    _ <- verify "two_dw_vert_concat" two_dw_vert_concat
    _ <- verify "horiz_concat_dw_assoc" horiz_concat_dw_assoc
    liftIO $ putStrLn $ replicate 80 '-'
    _ <- verify "single row vert concats to correct height" vert_contat_single_row
    _ <- verify "disjoint_height_horiz_join" disjoint_height_horiz_join
    _ <- verify "disjoint_height_horiz_join BG fill" disjoint_height_horiz_join_bg_fill
    _ <- verify "disjoint_width_vert_join" disjoint_width_vert_join
    _ <- verify "disjoint_width_vert_join BG fill" disjoint_width_vert_join_bg_fill
    return ()

