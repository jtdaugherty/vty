{-# LANGUAGE ScopedTypeVariables #-}
module VerifyImageOps where

import Graphics.Vty.Attributes
import Graphics.Vty.Image.Internal
import Verify.Graphics.Vty.Image

import Verify

import Control.DeepSeq

twoSwHorizConcat :: SingleColumnChar -> SingleColumnChar -> Bool
twoSwHorizConcat (SingleColumnChar c1) (SingleColumnChar c2) =
    imageWidth (char defAttr c1 <|> char defAttr c2) == 2

manySwHorizConcat :: [SingleColumnChar] -> Bool
manySwHorizConcat cs =
    let chars = [ char | SingleColumnChar char <- cs ]
        l = fromIntegral $ length cs
    in imageWidth ( horizCat $ map (char defAttr) chars ) == l

twoSwVertConcat :: SingleColumnChar -> SingleColumnChar -> Bool
twoSwVertConcat (SingleColumnChar c1) (SingleColumnChar c2) =
    imageHeight (char defAttr c1 <-> char defAttr c2) == 2

horizConcatSwAssoc :: SingleColumnChar -> SingleColumnChar -> SingleColumnChar -> Bool
horizConcatSwAssoc (SingleColumnChar c0) (SingleColumnChar c1) (SingleColumnChar c2) =
    (char defAttr c0 <|> char defAttr c1) <|> char defAttr c2
    ==
    char defAttr c0 <|> (char defAttr c1 <|> char defAttr c2)

twoDwHorizConcat :: DoubleColumnChar -> DoubleColumnChar -> Bool
twoDwHorizConcat (DoubleColumnChar c1) (DoubleColumnChar c2) =
    imageWidth (char defAttr c1 <|> char defAttr c2) == 4

manyDwHorizConcat :: [DoubleColumnChar] -> Bool
manyDwHorizConcat cs =
    let chars = [ char | DoubleColumnChar char <- cs ]
        l = fromIntegral $ length cs
    in imageWidth ( horizCat $ map (char defAttr) chars ) == l * 2

twoDwVertConcat :: DoubleColumnChar -> DoubleColumnChar -> Bool
twoDwVertConcat (DoubleColumnChar c1) (DoubleColumnChar c2) =
    imageHeight (char defAttr c1 <-> char defAttr c2) == 2

horizConcatDwAssoc :: DoubleColumnChar -> DoubleColumnChar -> DoubleColumnChar -> Bool
horizConcatDwAssoc (DoubleColumnChar c0) (DoubleColumnChar c1) (DoubleColumnChar c2) =
    (char defAttr c0 <|> char defAttr c1) <|> char defAttr c2
    ==
    char defAttr c0 <|> (char defAttr c1 <|> char defAttr c2)

vertContatSingleRow :: NonEmptyList SingleRowSingleAttrImage -> Bool
vertContatSingleRow (NonEmpty stack) =
    let expectedHeight :: Int = length stack
        stackImage = vertCat [ i | SingleRowSingleAttrImage { rowImage = i } <- stack ]
    in imageHeight stackImage == expectedHeight

disjointHeightHorizJoin :: NonEmptyList SingleRowSingleAttrImage
                        -> NonEmptyList SingleRowSingleAttrImage
                        -> Bool
disjointHeightHorizJoin (NonEmpty stack0) (NonEmpty stack1) =
    let expectedHeight :: Int = max (length stack0) (length stack1)
        stackImage0 = vertCat [ i | SingleRowSingleAttrImage { rowImage = i } <- stack0 ]
        stackImage1 = vertCat [ i | SingleRowSingleAttrImage { rowImage = i } <- stack1 ]
    in imageHeight (stackImage0 <|> stackImage1) == expectedHeight


disjointHeightHorizJoinBgFill :: NonEmptyList SingleRowSingleAttrImage
                              -> NonEmptyList SingleRowSingleAttrImage
                              -> Bool
disjointHeightHorizJoinBgFill (NonEmpty stack0) (NonEmpty stack1) =
    let stackImage0 = vertCat [ i | SingleRowSingleAttrImage { rowImage = i } <- stack0 ]
        stackImage1 = vertCat [ i | SingleRowSingleAttrImage { rowImage = i } <- stack1 ]
        image = stackImage0 <|> stackImage1
        expectedHeight = imageHeight image
    in case image of
        HorizJoin {}  -> ( expectedHeight == (imageHeight $ partLeft image) )
                         &&
                         ( expectedHeight == (imageHeight $ partRight image) )
        _             -> True

disjointWidthVertJoin :: NonEmptyList SingleRowSingleAttrImage
                      -> NonEmptyList SingleRowSingleAttrImage
                      -> Bool
disjointWidthVertJoin (NonEmpty stack0) (NonEmpty stack1) =
    let expectedWidth = maximum $ map imageWidth (stack0Images ++ stack1Images)
        stack0Images = [ i | SingleRowSingleAttrImage { rowImage = i } <- stack0 ]
        stack1Images = [ i | SingleRowSingleAttrImage { rowImage = i } <- stack1 ]
        stack0Image = vertCat stack0Images
        stack1Image = vertCat stack1Images
        image = stack0Image <-> stack1Image
    in imageWidth image == expectedWidth

disjointWidthVertJoinBgFill :: NonEmptyList SingleRowSingleAttrImage
                            -> NonEmptyList SingleRowSingleAttrImage
                            -> Bool
disjointWidthVertJoinBgFill (NonEmpty stack0) (NonEmpty stack1) =
    let expectedWidth = maximum $ map imageWidth (stack0Images ++ stack1Images)
        stack0Images = [ i | SingleRowSingleAttrImage { rowImage = i } <- stack0 ]
        stack1Images = [ i | SingleRowSingleAttrImage { rowImage = i } <- stack1 ]
        stack0Image = vertCat stack0Images
        stack1Image = vertCat stack1Images
        image = stack0Image <-> stack1Image
    in case image of
        VertJoin {} -> ( expectedWidth == (imageWidth $ partTop image) )
                       &&
                       ( expectedWidth == (imageWidth $ partBottom image) )
        _           -> True

translationIsLinearOnOutSize :: Translation -> Bool
translationIsLinearOnOutSize (Translation i (x,y) i') =
    imageWidth i' == imageWidth i + x && imageHeight i' == imageHeight i + y

paddingIsLinearOnOutSize :: Image -> Gen Bool
paddingIsLinearOnOutSize i = do
    l <- offset
    t <- offset
    r <- offset
    b <- offset
    let i' = pad l t r b i
    return $ imageWidth i' == imageWidth i + l + r && imageHeight i' == imageHeight i + t + b
    where offset = choose (1,1024)

cropLeftLimitsWidth :: Image -> Int -> Property
cropLeftLimitsWidth i v = v >= 0 ==>
    v >= imageWidth (cropLeft v i)

cropRightLimitsWidth :: Image -> Int -> Property
cropRightLimitsWidth i v = v >= 0 ==>
    v >= imageWidth (cropRight v i)

cropTopLimitsHeight :: Image -> Int -> Property
cropTopLimitsHeight i v = v >= 0 ==>
    v >= imageHeight (cropTop v i)

cropBottomLimitsHeight :: Image -> Int -> Property
cropBottomLimitsHeight i v = v >= 0 ==>
    v >= imageHeight (cropBottom v i)

-- ridiculous tests just to satisfy my desire for nice code coverage :-P
canShowImage :: Image -> Bool
canShowImage i = length (show i) > 0

canRnfImage :: Image -> Bool
canRnfImage i = rnf i == ()

canPpImage :: Image -> Bool
canPpImage i = length (ppImageStructure i) > 0

tests :: IO [Test]
tests = return
    [ verify "twoSwHorizConcat" twoSwHorizConcat
    , verify "manySwHorizConcat" manySwHorizConcat
    , verify "twoSwVertConcat" twoSwVertConcat
    , verify "horizConcatSwAssoc" horizConcatSwAssoc
    , verify "manyDwHorizConcat" manyDwHorizConcat
    , verify "twoDwHorizConcat" twoDwHorizConcat
    , verify "twoDwVertConcat" twoDwVertConcat
    , verify "horizConcatDwAssoc" horizConcatDwAssoc
    , verify "single row vert concats to correct height" vertContatSingleRow
    , verify "disjointHeightHorizJoin" disjointHeightHorizJoin
    , verify "disjointHeightHorizJoin BG fill" disjointHeightHorizJoinBgFill
    , verify "disjointWidthVertJoin" disjointWidthVertJoin
    , verify "disjointWidthVertJoin BG fill" disjointWidthVertJoinBgFill
    , verify "translation effects output dimensions linearly" translationIsLinearOnOutSize
    , verify "padding effects output dimensions linearly" paddingIsLinearOnOutSize
    , verify "crop left limits width" cropLeftLimitsWidth
    , verify "crop right limits width" cropRightLimitsWidth
    , verify "crop top limits height" cropTopLimitsHeight
    , verify "crop bottom limits height" cropBottomLimitsHeight
    , verify "can show image" canShowImage
    , verify "can rnf image" canRnfImage
    , verify "can pp image" canPpImage
    ]
