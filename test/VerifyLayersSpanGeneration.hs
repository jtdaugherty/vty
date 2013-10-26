{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module VerifyLayersSpanGeneration where

import Verify.Graphics.Vty.DisplayRegion
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Span

import Graphics.Vty.Debug
import Graphics.Vty.PictureToSpans

import Verify

import qualified Data.Vector as Vector 

larger_horiz_span_occlusion :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
larger_horiz_span_occlusion row_0 row_1 =
    let i_0 = row_image row_0
        i_1 = row_image row_1
        (i_larger, i_smaller) = if image_width i_0 > image_width i_1 then (i_0, i_1) else (i_1, i_0)
        expected_ops = display_ops_for_image i_larger
        p = pic_for_layers [i_larger, i_smaller]
        ops = display_ops_for_pic p $ DisplayRegion (image_width i_larger) (image_height i_larger)
    in verify_ops_equality expected_ops ops

-- | Two rows stacked vertical is equivalent to the first row rendered as the top layer and the
-- second row rendered as a bottom layer with a background fill where the first row would be.
vert_stack_layer_equivalence_0 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
vert_stack_layer_equivalence_0 row_0 row_1 =
    let i_0 = row_image row_0
        i_1 = row_image row_1
        i = i_0 <-> i_1
        p = pic_for_image i
        i_lower = background_fill (image_width i_0) 1 <-> i_1
        p_layered = pic_for_layers [i_0, i_lower]
        expected_ops = display_ops_for_image i
        ops_layered = display_ops_for_pic p_layered $ DisplayRegion (image_width i_lower) (image_height i_lower)
    in verify_ops_equality expected_ops ops_layered

vert_stack_layer_equivalence_1 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
vert_stack_layer_equivalence_1 row_0 row_1 =
    let i_0 = row_image row_0
        i_1 = row_image row_1
        i = i_0 <-> i_1
        p = pic_for_image i
        i_lower = i_0 <-> background_fill (image_width i_1) 1
        i_upper = background_fill (image_width i_0) 1 <-> i_1
        p_layered = pic_for_layers [i_upper, i_lower]
        expected_ops = display_ops_for_image i
        ops_layered = display_ops_for_pic p_layered $ DisplayRegion (image_width i_lower) (image_height i_lower)
    in verify_ops_equality expected_ops ops_layered

-- | Two rows horiz joined is equivalent to the first row rendered as the top layer and the
-- second row rendered as a bottom layer with a background fill where the first row would be.
horiz_join_layer_equivalence_0 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
horiz_join_layer_equivalence_0 row_0 row_1 =
    let i_0 = row_image row_0
        i_1 = row_image row_1
        i = i_0 <|> i_1
        p = pic_for_image i
        i_lower = background_fill (image_width i_0) 1 <|> i_1
        p_layered = pic_for_layers [i_0, i_lower]
        expected_ops = display_ops_for_image i
        ops_layered = display_ops_for_pic p_layered $ DisplayRegion (image_width i_lower) (image_height i_lower)
    in verify_ops_equality expected_ops ops_layered

horiz_join_layer_equivalence_1 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
horiz_join_layer_equivalence_1 row_0 row_1 =
    let i_0 = row_image row_0
        i_1 = row_image row_1
        i = i_0 <|> i_1
        p = pic_for_image i
        i_lower = i_0 <|> background_fill (image_width i_1) 1
        i_upper = background_fill (image_width i_0) 1 <|> i_1
        p_layered = pic_for_layers [i_upper, i_lower]
        expected_ops = display_ops_for_image i
        ops_layered = display_ops_for_pic p_layered $ DisplayRegion (image_width i_lower) (image_height i_lower)
    in verify_ops_equality expected_ops ops_layered

horiz_join_alternate_0 :: Result
horiz_join_alternate_0 =
    let size = 4
        str_0 = replicate size 'a'
        str_1 = replicate size 'b'
        i_0 = string def_attr str_0
        i_1 = string def_attr str_1
        i = horiz_cat $ zipWith horiz_join (replicate size i_0) (replicate size i_1)
        layer_0 = horiz_cat $ replicate size $ i_0 <|> background_fill size 1
        layer_1 = horiz_cat $ replicate size $ background_fill size 1 <|> i_1
        expected_ops = display_ops_for_image i
        ops_layered = display_ops_for_pic (pic_for_layers [layer_0, layer_1])
                                          (DisplayRegion (image_width i) (image_height i))
    in verify_ops_equality expected_ops ops_layered

horiz_join_alternate_1 :: Result
horiz_join_alternate_1 =
    let size = 4
        str_0 = replicate size 'a'
        str_1 = replicate size 'b'
        i_0 = string def_attr str_0
        i_1 = string def_attr str_1
        i = horiz_cat $ zipWith horiz_join (replicate size i_0) (replicate size i_1)
        layers = [l | b <- take 4 [0,size*2..], let l = background_fill b 1 <|> i_0 <|> i_1]
        expected_ops = display_ops_for_image i
        ops_layered = display_ops_for_pic (pic_for_layers layers)
                                          (DisplayRegion (image_width i) (image_height i))
    in verify_ops_equality expected_ops ops_layered

tests :: IO [Test]
tests = return 
    [ verify "a larger horiz span occludes a smaller span on a lower layer"
        larger_horiz_span_occlusion
    , verify "two rows stack vertical equiv to first image layered on top of second with padding (0)"
        vert_stack_layer_equivalence_0
    , verify "two rows stack vertical equiv to first image layered on top of second with padding (1)"
        vert_stack_layer_equivalence_1
    -- , verify "two rows horiz joined equiv to first image layered on top of second with padding (0)"
    --     horiz_join_layer_equivalence_0
    -- , verify "two rows horiz joined equiv to first image layered on top of second with padding (1)"
    --     horiz_join_layer_equivalence_1
    -- , verify "alternating images using joins is the same as alternating images using layers (0)"
    --     horiz_join_alternate_0
    -- , verify "alternating images using joins is the same as alternating images using layers (1)"
    --     horiz_join_alternate_1
    ]

