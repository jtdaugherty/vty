{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module VerifySimpleSpanGeneration where

import Verify.Graphics.Vty.Prelude

import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Span

import Graphics.Vty.Debug
import Graphics.Vty.PictureToSpans

import Verify

import qualified Data.Vector as Vector

unitImageAndZeroWindow0 :: UnitImage -> EmptyWindow -> Bool
unitImageAndZeroWindow0 (UnitImage _ i) (EmptyWindow w) =
    let p = picForImage i
        ops = displayOpsForPic p (regionForWindow w)
    in displayOpsColumns ops == 0 && displayOpsRows ops == 0

unitImageAndZeroWindow1 :: UnitImage -> EmptyWindow -> Bool
unitImageAndZeroWindow1 (UnitImage _ i) (EmptyWindow w) =
    let p = picForImage i
        ops = displayOpsForPic p (regionForWindow w)
    in ( spanOpsEffectedRows ops == 0 ) && ( allSpansHaveWidth ops 0 )

horizSpanImageAndZeroWindow0 :: SingleRowSingleAttrImage -> EmptyWindow -> Bool
horizSpanImageAndZeroWindow0 (SingleRowSingleAttrImage { rowImage = i }) (EmptyWindow w) =
    let p = picForImage i
        ops = displayOpsForPic p (regionForWindow w)
    in displayOpsColumns ops == 0 && displayOpsRows ops == 0

horizSpanImageAndZeroWindow1 :: SingleRowSingleAttrImage -> EmptyWindow -> Bool
horizSpanImageAndZeroWindow1 (SingleRowSingleAttrImage { rowImage = i }) (EmptyWindow w) =
    let p = picForImage i
        ops = displayOpsForPic p (regionForWindow w)
    in ( spanOpsEffectedRows ops == 0 ) && ( allSpansHaveWidth ops 0 )

horizSpanImageAndEqualWindow0 :: SingleRowSingleAttrImage -> Result
horizSpanImageAndEqualWindow0 (SingleRowSingleAttrImage { rowImage = i, expectedColumns = c }) =
    let p = picForImage i
        w = MockWindow c 1
        ops = displayOpsForPic p (regionForWindow w)
    in verifyAllSpansHaveWidth i ops c

horizSpanImageAndEqualWindow1 :: SingleRowSingleAttrImage -> Bool
horizSpanImageAndEqualWindow1 (SingleRowSingleAttrImage { rowImage = i, expectedColumns = c }) =
    let p = picForImage i
        w = MockWindow c 1
        ops = displayOpsForPic p (regionForWindow w)
    in spanOpsEffectedRows ops == 1

horizSpanImageAndLesserWindow0 :: SingleRowSingleAttrImage -> Result
horizSpanImageAndLesserWindow0 (SingleRowSingleAttrImage { rowImage = i, expectedColumns = c }) =
    let p = picForImage i
        lesserWidth = c `div` 2
        w = MockWindow lesserWidth 1
        ops = displayOpsForPic p (regionForWindow w)
    in verifyAllSpansHaveWidth i ops lesserWidth

singleAttrSingleSpanStackCropped0 :: SingleAttrSingleSpanStack -> Result
singleAttrSingleSpanStackCropped0 stack =
    let p = picForImage (stackImage stack)
        w = MockWindow (stackWidth stack `div` 2) (stackHeight stack)
        ops = displayOpsForPic p (regionForWindow w)
    in verifyAllSpansHaveWidth (stackImage stack) ops (stackWidth stack `div` 2)

singleAttrSingleSpanStackCropped1 :: SingleAttrSingleSpanStack -> Bool
singleAttrSingleSpanStackCropped1 stack =
    let p = picForImage (stackImage stack)
        expectedRowCount = stackHeight stack `div` 2
        w = MockWindow (stackWidth stack) expectedRowCount
        ops = displayOpsForPic p (regionForWindow w)
        actualRowCount = spanOpsEffectedRows ops
    in expectedRowCount == actualRowCount

singleAttrSingleSpanStackCropped2 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Result
singleAttrSingleSpanStackCropped2 stack0 stack1 =
    let p = picForImage (stackImage stack0 <|> stackImage stack1)
        w = MockWindow (stackWidth stack0) (imageHeight (picImage p))
        ops = displayOpsForPic p (regionForWindow w)
    in verifyAllSpansHaveWidth (picImage p) ops (stackWidth stack0)

singleAttrSingleSpanStackCropped3 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Bool
singleAttrSingleSpanStackCropped3 stack0 stack1 =
    let p = picForImage (stackImage stack0 <|> stackImage stack1)
        w = MockWindow (imageWidth (picImage p))  expectedRowCount
        ops = displayOpsForPic p (regionForWindow w)
        expectedRowCount = imageHeight (picImage p) `div` 2
        actualRowCount = spanOpsEffectedRows ops
    in expectedRowCount == actualRowCount

singleAttrSingleSpanStackCropped4 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Result
singleAttrSingleSpanStackCropped4 stack0 stack1 =
    let p = picForImage (stackImage stack0 <-> stackImage stack1)
        w = MockWindow expectedWidth (imageHeight (picImage p))
        ops = displayOpsForPic p (regionForWindow w)
        expectedWidth = imageWidth (picImage p) `div` 2
    in verifyAllSpansHaveWidth (picImage p) ops expectedWidth

singleAttrSingleSpanStackCropped5 :: SingleAttrSingleSpanStack -> SingleAttrSingleSpanStack -> Bool
singleAttrSingleSpanStackCropped5 stack0 stack1 =
    let p = picForImage (stackImage stack0 <-> stackImage stack1)
        w = MockWindow (imageWidth (picImage p)) (stackHeight stack0)
        ops = displayOpsForPic p (regionForWindow w)
        expectedRowCount = stackHeight stack0
        actualRowCount = spanOpsEffectedRows ops
    in expectedRowCount == actualRowCount

horizSpanImageAndGreaterWindow0 :: SingleRowSingleAttrImage -> Result
horizSpanImageAndGreaterWindow0 (SingleRowSingleAttrImage { rowImage = i, expectedColumns = c }) =
    let p = picForImage i
        -- SingleRowSingleAttrImage always has width >= 1
        greaterWidth = c * 2
        w = MockWindow greaterWidth 1
        ops = displayOpsForPic p (regionForWindow w)
    in verifyAllSpansHaveWidth i ops greaterWidth

arbImageIsCropped :: DefaultImage -> MockWindow -> Bool
arbImageIsCropped (DefaultImage image) win@(MockWindow w h) =
    let pic = picForImage image
        ops = displayOpsForPic pic (regionForWindow win)
    in ( spanOpsEffectedRows ops == h ) && ( allSpansHaveWidth ops w )

spanOpsActuallyFillRows :: DefaultPic -> Bool
spanOpsActuallyFillRows (DefaultPic pic win) =
    let ops = displayOpsForPic pic (regionForWindow win)
        expectedRowCount = regionHeight (regionForWindow win)
        actualRowCount = spanOpsEffectedRows ops
    in expectedRowCount == actualRowCount

spanOpsActuallyFillColumns :: DefaultPic -> Bool
spanOpsActuallyFillColumns (DefaultPic pic win) =
    let ops = displayOpsForPic pic (regionForWindow win)
        expectedColumnCount = regionWidth (regionForWindow win)
    in allSpansHaveWidth ops expectedColumnCount

firstSpanOpSetsAttr :: DefaultPic -> Bool
firstSpanOpSetsAttr DefaultPic { defaultPic = pic, defaultWin = win } =
    let ops = displayOpsForPic pic (regionForWindow win)
    in all ( isAttrSpanOp . Vector.head ) ( Vector.toList ops )

singleAttrSingleSpanStackOpCoverage ::  SingleAttrSingleSpanStack -> Result
singleAttrSingleSpanStackOpCoverage stack =
    let p = picForImage (stackImage stack)
        w = MockWindow (stackWidth stack) (stackHeight stack)
        ops = displayOpsForPic p (regionForWindow w)
    in verifyAllSpansHaveWidth (stackImage stack) ops (stackWidth stack)

imageCoverageMatchesBounds :: Image -> Result
imageCoverageMatchesBounds i =
    let p = picForImage i
        r = (imageWidth i,imageHeight i)
        ops = displayOpsForPic p r
    in verifyAllSpansHaveWidth i ops (imageWidth i)

tests :: IO [Test]
tests = return
    [ verify "unit image is cropped when window size == (0,0) [0]" unitImageAndZeroWindow0
    , verify "unit image is cropped when window size == (0,0) [1]" unitImageAndZeroWindow1
    , verify "horiz span image is cropped when window size == (0,0) [0]" horizSpanImageAndZeroWindow0
    , verify "horiz span image is cropped when window size == (0,0) [1]" horizSpanImageAndZeroWindow1
    , verify "horiz span image is not cropped when window size == size of image [width]" horizSpanImageAndEqualWindow0
    , verify "horiz span image is not cropped when window size == size of image [height]" horizSpanImageAndEqualWindow1
    , verify "horiz span image is not cropped when window size < size of image [width]" horizSpanImageAndLesserWindow0
    , verify "horiz span image is not cropped when window size > size of image [width]" horizSpanImageAndGreaterWindow0
    , verify "first span op is always to set the text attribute" firstSpanOpSetsAttr
    , verify "a stack of single attr text spans should define content for all the columns [output region == size of stack]"
        singleAttrSingleSpanStackOpCoverage
    , verify "a single attr text span is cropped when window size < size of stack image [width]"
        singleAttrSingleSpanStackCropped0
    , verify "a single attr text span is cropped when window size < size of stack image [height]"
        singleAttrSingleSpanStackCropped1
    , verify "single attr text span <|> single attr text span display cropped. [width]"
        singleAttrSingleSpanStackCropped2
    , verify "single attr text span <|> single attr text span display cropped. [height]"
        singleAttrSingleSpanStackCropped3
    , verify "single attr text span <-> single attr text span display cropped. [width]"
        singleAttrSingleSpanStackCropped4
    , verify "single attr text span <-> single attr text span display cropped. [height]"
        singleAttrSingleSpanStackCropped5
    , verify "an arbitrary image when rendered to a window of the same size will cover the entire window"
        imageCoverageMatchesBounds
    ]

