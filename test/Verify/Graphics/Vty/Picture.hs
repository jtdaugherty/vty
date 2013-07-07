{-# LANGUAGE DisambiguateRecordFields #-}
module Verify.Graphics.Vty.Picture ( module Verify.Graphics.Vty.Picture
                                   , module Graphics.Vty.Picture
                                   )
    where

import Graphics.Vty.Picture
import Graphics.Vty.Debug

import Verify.Graphics.Vty.Attributes
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.DisplayRegion

import Verify

data DefaultPic = DefaultPic 
    { default_pic :: Picture
    , default_win :: MockWindow 
    }

instance Show DefaultPic where
    show (DefaultPic pic win)
        = "DefaultPic\n\t( " ++ show pic ++ ")\n\t" ++ show win ++ "\n"

instance Arbitrary DefaultPic where
    arbitrary = do
        DefaultImage image <- arbitrary
        let win = MockWindow (image_width image) (image_height image)
        return $ DefaultPic (pic_for_image image) 
                            win 

data PicWithBGAttr = PicWithBGAttr 
    { with_attr_pic :: Picture
    , with_attr_win :: MockWindow
    , with_attr_specified_attr :: Attr
    } deriving ( Show )

instance Arbitrary PicWithBGAttr where
    arbitrary = do
        DefaultImage image <- arbitrary
        let win = MockWindow (image_width image) (image_height image)
        attr <- arbitrary
        return $ PicWithBGAttr (pic_for_image image) 
                               win 
                               attr
        
