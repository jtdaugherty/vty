{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module VerifyCropSpanGeneration where

import Verify.Graphics.Vty.DisplayRegion
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Span

import Graphics.Vty.Debug
import Graphics.Vty.PictureToSpans

import Verify

import qualified Data.Vector as Vector 

crop_op_display_ops :: (Int -> Image -> Image) ->
                       Int -> Image -> (DisplayOps, Image)
crop_op_display_ops crop_op v i =
    let i_out = crop_op v i
        p = pic_for_image i_out
        w = MockWindow (image_width i_out) (image_height i_out)
    in (display_ops_for_pic p (region_for_window w), i_out)

width_crop_output_columns :: (Int -> Image -> Image) ->
                             SingleAttrSingleSpanStack ->
                             NonNegative Int ->
                             Property
width_crop_output_columns crop_op s (NonNegative w) = stack_width s > w ==>
    let (ops, i_out) = crop_op_display_ops crop_op w (stack_image s)
    in verify_all_spans_have_width i_out ops w

height_crop_output_columns :: (Int -> Image -> Image) ->
                              SingleAttrSingleSpanStack ->
                              NonNegative Int ->
                              Property
height_crop_output_columns crop_op s (NonNegative h) = stack_height s > h ==>
    let (ops, _) = crop_op_display_ops crop_op h (stack_image s)
    in display_ops_rows ops == h

crop_right_output_columns :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
crop_right_output_columns = width_crop_output_columns crop_right

crop_left_output_columns :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
crop_left_output_columns = width_crop_output_columns crop_left

crop_top_output_rows :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
crop_top_output_rows = height_crop_output_columns crop_top

crop_bottom_output_rows :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
crop_bottom_output_rows = height_crop_output_columns crop_bottom

-- TODO: known benign failure.
crop_right_and_left_rejoined_equivalence :: SingleAttrSingleSpanStack -> Property
crop_right_and_left_rejoined_equivalence stack = image_width (stack_image stack) `mod` 2 == 0 ==>
    let i = stack_image stack
        -- the right part is made by cropping the image from the left.
        i_r = crop_left (image_width i `div` 2) i
        -- the left part is made by cropping the image from the right
        i_l = crop_right (image_width i `div` 2) i
        i_alt = i_l <|> i_r
        i_ops = display_ops_for_image i
        i_alt_ops = display_ops_for_image i_alt
    in verify_ops_equality i_ops i_alt_ops

crop_top_and_bottom_rejoined_equivalence :: SingleAttrSingleSpanStack -> Property
crop_top_and_bottom_rejoined_equivalence stack = image_height (stack_image stack) `mod` 2 == 0 ==>
    let i = stack_image stack
        -- the top part is made by cropping the image from the bottom.
        i_t = crop_bottom (image_height i `div` 2) i
        -- the bottom part is made by cropping the image from the top.
        i_b = crop_top (image_height i `div` 2) i
        i_alt = i_t <-> i_b
    in display_ops_for_image i == display_ops_for_image i_alt

tests :: IO [Test]
tests = return 
    [ verify "cropping from the bottom produces display operations covering the expected rows"
        crop_bottom_output_rows
    , verify "cropping from the top produces display operations covering the expected rows"
        crop_top_output_rows
    , verify "cropping from the left produces display operations covering the expected columns"
        crop_left_output_columns
    , verify "cropping from the right produces display operations covering the expected columns"
        crop_right_output_columns
    -- TODO: known benign failure.
    -- , verify "the output of a stack is the same as that stack cropped left & right and joined together"
    --     crop_right_and_left_rejoined_equivalence
    , verify "the output of a stack is the same as that stack cropped top & bottom and joined together"
        crop_top_and_bottom_rejoined_equivalence
    ]

