{-# LANGUAGE DisambiguateRecordFields #-}
module Verify.Graphics.Vty.Picture
  ( module Verify.Graphics.Vty.Picture
  , module Graphics.Vty.Picture
  )
where

import Verify.Graphics.Vty.Prelude

import Graphics.Vty.Picture
import Graphics.Vty.Debug
import Graphics.Vty.Span (DisplayOps)
import Graphics.Vty.PictureToSpans (displayOpsForPic)

import Verify.Graphics.Vty.Attributes
import Verify.Graphics.Vty.Image

import Verify

-- | Returns the DisplayOps for an image rendered to a window the size
-- of the image.
--
-- largely used only for debugging.
displayOpsForImage :: Image -> DisplayOps
displayOpsForImage i = displayOpsForPic (picForImage i) (imageWidth i, imageHeight i)

data DefaultPic = DefaultPic
    { defaultPic :: Picture
    , defaultWin :: MockWindow
    }

instance Show DefaultPic where
    show (DefaultPic pic win)
        = "DefaultPic\n\t( " ++ show pic ++ ")\n\t" ++ show win ++ "\n"

instance Arbitrary DefaultPic where
    arbitrary = do
        DefaultImage image <- arbitrary
        let win = MockWindow (imageWidth image) (imageHeight image)
        return $ DefaultPic (picForImage image)
                            win

data PicWithBGAttr = PicWithBGAttr
    { withAttrPic :: Picture
    , withAttrWin :: MockWindow
    , withAttrSpecifiedAttr :: Attr
    } deriving ( Show )

instance Arbitrary PicWithBGAttr where
    arbitrary = do
        DefaultImage image <- arbitrary
        let win = MockWindow (imageWidth image) (imageHeight image)
        attr <- arbitrary
        return $ PicWithBGAttr (picForImage image)
                               win
                               attr

instance Arbitrary Picture where
    arbitrary = do
        layers <- Verify.resize 20 (listOf1 arbitrary)
        return $ picForLayers layers
