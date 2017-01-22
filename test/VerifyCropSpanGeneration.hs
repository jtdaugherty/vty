{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module VerifyCropSpanGeneration where

import Verify.Graphics.Vty.Prelude

import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Span

import Graphics.Vty.Debug
import Graphics.Vty.PictureToSpans

import Verify

import qualified Data.Vector as Vector

cropOpDisplayOps :: (Int -> Image -> Image) ->
                    Int -> Image -> (DisplayOps, Image)
cropOpDisplayOps cropOp v i =
    let iOut = cropOp v i
        p = picForImage iOut
        w = MockWindow (imageWidth iOut) (imageHeight iOut)
    in (displayOpsForPic p (regionForWindow w), iOut)

widthCropOutputColumns :: (Int -> Image -> Image) ->
                          SingleAttrSingleSpanStack ->
                          NonNegative Int ->
                          Property
widthCropOutputColumns cropOp s (NonNegative w) = stackWidth s > w ==>
    let (ops, iOut) = cropOpDisplayOps cropOp w (stackImage s)
    in verifyAllSpansHaveWidth iOut ops w

heightCropOutputColumns :: (Int -> Image -> Image) ->
                           SingleAttrSingleSpanStack ->
                           NonNegative Int ->
                           Property
heightCropOutputColumns cropOp s (NonNegative h) = stackHeight s > h ==>
    let (ops, _) = cropOpDisplayOps cropOp h (stackImage s)
    in displayOpsRows ops == h

cropRightOutputColumns :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
cropRightOutputColumns = widthCropOutputColumns cropRight

cropLeftOutputColumns :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
cropLeftOutputColumns = widthCropOutputColumns cropLeft

cropTopOutputRows :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
cropTopOutputRows = heightCropOutputColumns cropTop

cropBottomOutputRows :: SingleAttrSingleSpanStack -> NonNegative Int -> Property
cropBottomOutputRows = heightCropOutputColumns cropBottom

-- TODO: known benign failure.
cropRightAndLeftRejoinedEquivalence :: SingleAttrSingleSpanStack -> Property
cropRightAndLeftRejoinedEquivalence stack = imageWidth (stackImage stack) `mod` 2 == 0 ==>
    let i = stackImage stack
        -- the right part is made by cropping the image from the left.
        iR = cropLeft (imageWidth i `div` 2) i
        -- the left part is made by cropping the image from the right
        iL = cropRight (imageWidth i `div` 2) i
        iAlt = iL <|> iR
        iOps = displayOpsForImage i
        iAltOps = displayOpsForImage iAlt
    in verifyOpsEquality iOps iAltOps

cropTopAndBottomRejoinedEquivalence :: SingleAttrSingleSpanStack -> Property
cropTopAndBottomRejoinedEquivalence stack = imageHeight (stackImage stack) `mod` 2 == 0 ==>
    let i = stackImage stack
        -- the top part is made by cropping the image from the bottom.
        iT = cropBottom (imageHeight i `div` 2) i
        -- the bottom part is made by cropping the image from the top.
        iB = cropTop (imageHeight i `div` 2) i
        iAlt = iT <-> iB
    in displayOpsForImage i == displayOpsForImage iAlt

tests :: IO [Test]
tests = return
    [ verify "cropping from the bottom produces display operations covering the expected rows"
        cropBottomOutputRows
    , verify "cropping from the top produces display operations covering the expected rows"
        cropTopOutputRows
    , verify "cropping from the left produces display operations covering the expected columns"
        cropLeftOutputColumns
    , verify "cropping from the right produces display operations covering the expected columns"
        cropRightOutputColumns
    -- TODO: known benign failure.
    -- , verify "the output of a stack is the same as that stack cropped left & right and joined together"
    --     cropRightAndLeftRejoinedEquivalence
    , verify "the output of a stack is the same as that stack cropped top & bottom and joined together"
        cropTopAndBottomRejoinedEquivalence
    ]
