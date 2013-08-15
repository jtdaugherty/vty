{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module VerifySimpleSpanGeneration where

import Verify.Graphics.Vty.DisplayRegion
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Span

import Graphics.Vty.Debug
import Graphics.Vty.PictureToSpans

import Verify

import qualified Data.Vector as Vector 

unit_image_and_zero_window_0 :: UnitImage -> EmptyWindow -> Bool
unit_image_and_zero_window_0 (UnitImage _ i) (EmptyWindow w) = 
    let p = pic_for_image i
        ops = display_ops_for_pic p (region_for_window w)
    in display_ops_columns ops == 0 && display_ops_rows ops == 0

unit_image_and_zero_window_1 :: UnitImage -> EmptyWindow -> Bool
unit_image_and_zero_window_1 (UnitImage _ i) (EmptyWindow w) = 
    let p = pic_for_image i
        ops = display_ops_for_pic p (region_for_window w)
    in ( span_ops_effected_rows ops == 0 ) && ( all_spans_have_width ops 0 )

horiz_span_image_and_zero_window_0 :: SingleRowSingleAttrImage -> EmptyWindow -> Bool
horiz_span_image_and_zero_window_0 (SingleRowSingleAttrImage { row_image = i }) (EmptyWindow w) = 
    let p = pic_for_image i
        ops = display_ops_for_pic p (region_for_window w)
    in display_ops_columns ops == 0 && display_ops_rows ops == 0

horiz_span_image_and_zero_window_1 :: SingleRowSingleAttrImage -> EmptyWindow -> Bool
horiz_span_image_and_zero_window_1 (SingleRowSingleAttrImage { row_image = i }) (EmptyWindow w) = 
    let p = pic_for_image i
        ops = display_ops_for_pic p (region_for_window w)
    in ( span_ops_effected_rows ops == 0 ) && ( all_spans_have_width ops 0 )

horiz_span_image_and_equal_window_0 :: SingleRowSingleAttrImage -> Result
horiz_span_image_and_equal_window_0 (SingleRowSingleAttrImage { row_image = i, expected_columns = c }) =
    let p = pic_for_image i
        w = MockWindow c 1
        ops = display_ops_for_pic p (region_for_window w)
    in verify_all_spans_have_width i ops c

horiz_span_image_and_equal_window_1 :: SingleRowSingleAttrImage -> Bool
horiz_span_image_and_equal_window_1 (SingleRowSingleAttrImage { row_image = i, expected_columns = c }) =
    let p = pic_for_image i
        w = MockWindow c 1
        ops = display_ops_for_pic p (region_for_window w)
    in span_ops_effected_rows ops == 1

horiz_span_image_and_lesser_window_0 :: SingleRowSingleAttrImage -> Result
horiz_span_image_and_lesser_window_0 (SingleRowSingleAttrImage { row_image = i, expected_columns = c }) =
    let p = pic_for_image i
        lesser_width = c `div` 2
        w = MockWindow lesser_width 1
        ops = display_ops_for_pic p (region_for_window w)
    in verify_all_spans_have_width i ops lesser_width

single_attr_single_span_stack_cropped_0 :: SingleAttrSingleSpanStack -> Result
single_attr_single_span_stack_cropped_0 stack =
    let p = pic_for_image (stack_image stack)
        w = MockWindow (stack_width stack `div` 2) (stack_height stack)
        ops = display_ops_for_pic p (region_for_window w)
    in verify_all_spans_have_width (stack_image stack) ops (stack_width stack `div` 2)

single_attr_single_span_stack_cropped_1 :: SingleAttrSingleSpanStack -> Bool
single_attr_single_span_stack_cropped_1 stack =
    let p = pic_for_image (stack_image stack)
        expected_row_count = stack_height stack `div` 2
        w = MockWindow (stack_width stack) expected_row_count
        ops = display_ops_for_pic p (region_for_window w)
        actual_row_count = span_ops_effected_rows ops
    in expected_row_count == actual_row_count

single_attr_single_span_stack_cropped_2 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Result
single_attr_single_span_stack_cropped_2 stack_0 stack_1 =
    let p = pic_for_image (stack_image stack_0 <|> stack_image stack_1)
        w = MockWindow (stack_width stack_0) (image_height (pic_image p))
        ops = display_ops_for_pic p (region_for_window w)
    in verify_all_spans_have_width (pic_image p) ops (stack_width stack_0)

single_attr_single_span_stack_cropped_3 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Bool
single_attr_single_span_stack_cropped_3 stack_0 stack_1 =
    let p = pic_for_image (stack_image stack_0 <|> stack_image stack_1)
        w = MockWindow (image_width (pic_image p))  expected_row_count
        ops = display_ops_for_pic p (region_for_window w)
        expected_row_count = image_height (pic_image p) `div` 2
        actual_row_count = span_ops_effected_rows ops
    in expected_row_count == actual_row_count

single_attr_single_span_stack_cropped_4 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Result
single_attr_single_span_stack_cropped_4 stack_0 stack_1 =
    let p = pic_for_image (stack_image stack_0 <-> stack_image stack_1)
        w = MockWindow expected_width (image_height (pic_image p))
        ops = display_ops_for_pic p (region_for_window w)
        expected_width = image_width (pic_image p) `div` 2
    in verify_all_spans_have_width (pic_image p) ops expected_width

single_attr_single_span_stack_cropped_5 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Bool
single_attr_single_span_stack_cropped_5 stack_0 stack_1 =
    let p = pic_for_image (stack_image stack_0 <-> stack_image stack_1)
        w = MockWindow (image_width (pic_image p)) (stack_height stack_0)
        ops = display_ops_for_pic p (region_for_window w)
        expected_row_count = stack_height stack_0
        actual_row_count = span_ops_effected_rows ops
    in expected_row_count == actual_row_count

horiz_span_image_and_greater_window_0 :: SingleRowSingleAttrImage -> Result
horiz_span_image_and_greater_window_0 (SingleRowSingleAttrImage { row_image = i, expected_columns = c }) =
    let p = pic_for_image i
        -- SingleRowSingleAttrImage always has width >= 1
        greater_width = c * 2
        w = MockWindow greater_width 1
        ops = display_ops_for_pic p (region_for_window w)
    in verify_all_spans_have_width i ops greater_width

arb_image_is_cropped :: DefaultImage -> MockWindow -> Bool
arb_image_is_cropped (DefaultImage image) win@(MockWindow w h) =
    let pic = pic_for_image image
        ops = display_ops_for_pic pic (region_for_window win)
    in ( span_ops_effected_rows ops == h ) && ( all_spans_have_width ops w )

span_ops_actually_fill_rows :: DefaultPic -> Bool
span_ops_actually_fill_rows (DefaultPic pic win) =
    let ops = display_ops_for_pic pic (region_for_window win)
        expected_row_count = region_height (region_for_window win)
        actual_row_count = span_ops_effected_rows ops
    in expected_row_count == actual_row_count

span_ops_actually_fill_columns :: DefaultPic -> Bool
span_ops_actually_fill_columns (DefaultPic pic win) =
    let ops = display_ops_for_pic pic (region_for_window win)
        expected_column_count = region_width (region_for_window win)
    in all_spans_have_width ops expected_column_count

first_span_op_sets_attr :: DefaultPic -> Bool
first_span_op_sets_attr DefaultPic { default_pic = pic, default_win = win } = 
    let ops = display_ops_for_pic pic (region_for_window win)
    in all ( is_attr_span_op . Vector.head ) ( Vector.toList ops )

single_attr_single_span_stack_op_coverage ::  SingleAttrSingleSpanStack -> Result
single_attr_single_span_stack_op_coverage stack =
    let p = pic_for_image (stack_image stack)
        w = MockWindow (stack_width stack) (stack_height stack)
        ops = display_ops_for_pic p (region_for_window w)
    in verify_all_spans_have_width (stack_image stack) ops (stack_width stack)

image_coverage_matches_bounds :: Image -> Result
image_coverage_matches_bounds i =
    let p = pic_for_image i
        r = DisplayRegion (image_width i) (image_height i)
        ops = display_ops_for_pic p r
    in verify_all_spans_have_width i ops (image_width i)

tests :: IO [Test]
tests = return 
    [ verify "unit image is cropped when window size == (0,0) [0]" unit_image_and_zero_window_0
    , verify "unit image is cropped when window size == (0,0) [1]" unit_image_and_zero_window_1
    , verify "horiz span image is cropped when window size == (0,0) [0]" horiz_span_image_and_zero_window_0
    , verify "horiz span image is cropped when window size == (0,0) [1]" horiz_span_image_and_zero_window_1
    , verify "horiz span image is not cropped when window size == size of image [width]" horiz_span_image_and_equal_window_0
    , verify "horiz span image is not cropped when window size == size of image [height]" horiz_span_image_and_equal_window_1
    , verify "horiz span image is not cropped when window size < size of image [width]" horiz_span_image_and_lesser_window_0
    , verify "horiz span image is not cropped when window size > size of image [width]" horiz_span_image_and_greater_window_0
    , verify "first span op is always to set the text attribute" first_span_op_sets_attr
    , verify "a stack of single attr text spans should define content for all the columns [output region == size of stack]"
        single_attr_single_span_stack_op_coverage
    , verify "a single attr text span is cropped when window size < size of stack image [width]"
        single_attr_single_span_stack_cropped_0 
    , verify "a single attr text span is cropped when window size < size of stack image [height]"
        single_attr_single_span_stack_cropped_1
    , verify "single attr text span <|> single attr text span display cropped. [width]"
        single_attr_single_span_stack_cropped_2
    , verify "single attr text span <|> single attr text span display cropped. [height]"
        single_attr_single_span_stack_cropped_3
    , verify "single attr text span <-> single attr text span display cropped. [width]"
        single_attr_single_span_stack_cropped_4
    , verify "single attr text span <-> single attr text span display cropped. [height]"
        single_attr_single_span_stack_cropped_5
    , verify "an arbitrary image when rendered to a window of the same size will cover the entire window"
        image_coverage_matches_bounds
    ]

