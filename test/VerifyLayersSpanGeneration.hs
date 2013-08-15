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

tests :: IO [Test]
tests = return 
    [ verify "a larger horiz span occludes a smaller span on a lower layer"
        larger_horiz_span_occlusion
    ]

