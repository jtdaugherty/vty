{-# LANGUAGE NamedFieldPuns #-}
module VerifyImageTrans where

import Verify.Graphics.Vty.Image

import Graphics.Vty.Image.Internal

import Verify

import Data.Word

isHorizTextOfColumns :: Image -> Int -> Bool
isHorizTextOfColumns (HorizText { outputWidth = inW }) expectedW = inW == expectedW
isHorizTextOfColumns (BGFill { outputWidth = inW }) expectedW = inW == expectedW
isHorizTextOfColumns _image _expectedW = False

verifyHorizContatWoAttrChangeSimplifies :: SingleRowSingleAttrImage -> Bool
verifyHorizContatWoAttrChangeSimplifies (SingleRowSingleAttrImage _attr charCount image) =
    isHorizTextOfColumns image charCount

verifyHorizContatWAttrChangeSimplifies :: SingleRowTwoAttrImage -> Bool
verifyHorizContatWAttrChangeSimplifies ( SingleRowTwoAttrImage (SingleRowSingleAttrImage attr0 charCount0 _image0)
                                                               (SingleRowSingleAttrImage attr1 charCount1 _image1)
                                                               i
                                             ) 
    | charCount0 == 0 || charCount1 == 0 || attr0 == attr1 = isHorizTextOfColumns i (charCount0 + charCount1)
    | otherwise = False == isHorizTextOfColumns i (charCount0 + charCount1)

tests :: IO [Test]
tests = return
    [ verify "verifyHorizContatWoAttrChangeSimplifies" verifyHorizContatWoAttrChangeSimplifies
    , verify "verifyHorizContatWAttrChangeSimplifies" verifyHorizContatWAttrChangeSimplifies
    ]

