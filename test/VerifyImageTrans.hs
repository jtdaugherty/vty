{-# LANGUAGE NamedFieldPuns #-}
module VerifyImageTrans where

import Verify.Graphics.Vty.Image

import Graphics.Vty.Image.Internal

import Verify

import Data.Word

is_horiz_text_of_columns :: Image -> Int -> Bool
is_horiz_text_of_columns (HorizText { output_width = in_w }) expected_w = in_w == expected_w
is_horiz_text_of_columns (BGFill { output_width = in_w }) expected_w = in_w == expected_w
is_horiz_text_of_columns _image _expected_w = False

verify_horiz_contat_wo_attr_change_simplifies :: SingleRowSingleAttrImage -> Bool
verify_horiz_contat_wo_attr_change_simplifies (SingleRowSingleAttrImage _attr char_count image) =
    is_horiz_text_of_columns image char_count

verify_horiz_contat_w_attr_change_simplifies :: SingleRowTwoAttrImage -> Bool
verify_horiz_contat_w_attr_change_simplifies ( SingleRowTwoAttrImage (SingleRowSingleAttrImage attr0 char_count0 _image0)
                                                                     (SingleRowSingleAttrImage attr1 char_count1 _image1)
                                                                     i
                                             ) 
    | char_count0 == 0 || char_count1 == 0 || attr0 == attr1 = is_horiz_text_of_columns i (char_count0 + char_count1)
    | otherwise = False == is_horiz_text_of_columns i (char_count0 + char_count1)

tests :: IO [Test]
tests = return
    [ verify "verify_horiz_contat_wo_attr_change_simplifies" verify_horiz_contat_wo_attr_change_simplifies
    , verify "verify_horiz_contat_w_attr_change_simplifies" verify_horiz_contat_w_attr_change_simplifies
    ]

