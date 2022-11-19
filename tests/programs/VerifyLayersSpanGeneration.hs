{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module VerifyLayersSpanGeneration where

import Verify.Graphics.Vty.Prelude

import Verify.Graphics.Vty.Attributes
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Span

import Graphics.Vty.Debug
import Graphics.Vty.PictureToSpans

import Verify

import qualified Data.Vector as Vector

largerHorizSpanOcclusion :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
largerHorizSpanOcclusion row0 row1 =
    let i0 = rowImage row0
        i1 = rowImage row1
        (iLarger, iSmaller) = if imageWidth i0 > imageWidth i1 then (i0, i1) else (i1, i0)
        expectedOps = displayOpsForImage iLarger
        p = picForLayers [iLarger, iSmaller]
        ops = displayOpsForPic p (imageWidth iLarger,imageHeight iLarger)
    in verifyOpsEquality expectedOps ops

-- | Two rows stacked vertical is equivalent to the first row rendered
-- as the top layer and the second row rendered as a bottom layer with a
-- background fill where the first row would be.
vertStackLayerEquivalence0 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
vertStackLayerEquivalence0 row0 row1 =
    let i0 = rowImage row0
        i1 = rowImage row1
        i = i0 <-> i1
        p = picForImage i
        iLower = backgroundFill (imageWidth i0) 1 <-> i1
        pLayered = picForLayers [i0, iLower]
        expectedOps = displayOpsForImage i
        opsLayered = displayOpsForPic pLayered (imageWidth iLower,imageHeight iLower)
    in verifyOpsEquality expectedOps opsLayered

vertStackLayerEquivalence1 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
vertStackLayerEquivalence1 row0 row1 =
    let i0 = rowImage row0
        i1 = rowImage row1
        i = i0 <-> i1
        p = picForImage i
        iLower = i0 <-> backgroundFill (imageWidth i1) 1
        iUpper = backgroundFill (imageWidth i0) 1 <-> i1
        pLayered = picForLayers [iUpper, iLower]
        expectedOps = displayOpsForImage i
        opsLayered = displayOpsForPic pLayered (imageWidth iLower,imageHeight iLower)
    in verifyOpsEquality expectedOps opsLayered

-- | Two rows horiz joined is equivalent to the first row rendered as
-- the top layer and the second row rendered as a bottom layer with a
-- background fill where the first row would be.
horizJoinLayerEquivalence0 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
horizJoinLayerEquivalence0 row0 row1 =
    let i0 = rowImage row0
        i1 = rowImage row1
        i = i0 <|> i1
        p = picForImage i
        iLower = backgroundFill (imageWidth i0) 1 <|> i1
        pLayered = picForLayers [i0, iLower]
        expectedOps = displayOpsForImage i
        opsLayered = displayOpsForPic pLayered (imageWidth iLower,imageHeight iLower)
    in verifyOpsEquality expectedOps opsLayered

horizJoinLayerEquivalence1 :: SingleRowSingleAttrImage -> SingleRowSingleAttrImage -> Result
horizJoinLayerEquivalence1 row0 row1 =
    let i0 = rowImage row0
        i1 = rowImage row1
        i = i0 <|> i1
        p = picForImage i
        iLower = i0 <|> backgroundFill (imageWidth i1) 1
        iUpper = backgroundFill (imageWidth i0) 1 <|> i1
        pLayered = picForLayers [iUpper, iLower]
        expectedOps = displayOpsForImage i
        opsLayered = displayOpsForPic pLayered (imageWidth iLower,imageHeight iLower)
    in verifyOpsEquality expectedOps opsLayered

horizJoinAlternate0 :: Result
horizJoinAlternate0 =
    let size = 4
        str0 = replicate size 'a'
        str1 = replicate size 'b'
        i0 = string defAttr str0
        i1 = string defAttr str1
        i = horizCat $ zipWith horizJoin (replicate size i0) (replicate size i1)
        layer0 = horizCat $ replicate size $ i0 <|> backgroundFill size 1
        layer1 = horizCat $ replicate size $ backgroundFill size 1 <|> i1
        expectedOps = displayOpsForImage i
        opsLayered = displayOpsForPic (picForLayers [layer0, layer1])
                                      (imageWidth i,imageHeight i)
    in verifyOpsEquality expectedOps opsLayered

horizJoinAlternate1 :: Result
horizJoinAlternate1 =
    let size = 4
        str0 = replicate size 'a'
        str1 = replicate size 'b'
        i0 = string defAttr str0
        i1 = string defAttr str1
        i = horizCat $ zipWith horizJoin (replicate size i0) (replicate size i1)
        layers = [l | b <- take 4 [0,size*2..], let l = backgroundFill b 1 <|> i0 <|> i1]
        expectedOps = displayOpsForImage i
        opsLayered = displayOpsForPic (picForLayers layers)
                                      (imageWidth i,imageHeight i)
    in verifyOpsEquality expectedOps opsLayered

tests :: IO [Test]
tests = return
    [ verify "a larger horiz span occludes a smaller span on a lower layer"
        largerHorizSpanOcclusion
    , verify "two rows stack vertical equiv to first image layered on top of second with padding (0)"
        vertStackLayerEquivalence0
    , verify "two rows stack vertical equiv to first image layered on top of second with padding (1)"
        vertStackLayerEquivalence1
    -- , verify "two rows horiz joined equiv to first image layered on top of second with padding (0)"
    --     horizJoinLayerEquivalence0
    -- , verify "two rows horiz joined equiv to first image layered on top of second with padding (1)"
    --     horizJoinLayerEquivalence1
    -- , verify "alternating images using joins is the same as alternating images using layers (0)"
    --     horizJoinAlternate0
    -- , verify "alternating images using joins is the same as alternating images using layers (1)"
    --     horizJoinAlternate1
    ]
