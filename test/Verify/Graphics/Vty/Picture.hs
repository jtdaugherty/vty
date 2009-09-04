{-# LANGUAGE DisambiguateRecordFields #-}
module Verify.Graphics.Vty.Picture ( module Verify.Graphics.Vty.Picture
                                   , module Graphics.Vty.Picture
                                   )
    where

import Graphics.Vty.Picture
import Graphics.Vty.Debug

import Verify.Graphics.Vty.Attributes
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.WinRegion

import Verify

data DefaultPic = DefaultPic 
    { default_pic :: Picture
    , default_win :: DebugWindow 
    , default_construct_log :: ImageConstructLog
    }

instance Show DefaultPic where
    show (DefaultPic pic win image_log) 
        = "DefaultPic\n\t( " ++ show pic ++ ")\n\t" ++ show win ++ "\n\t" ++ show image_log ++ "\n"

instance Arbitrary DefaultPic where
    arbitrary = do
        DefaultImage image image_construct_events <- arbitrary
        let win = DebugWindow (image_width image) (image_height image)
        return $ DefaultPic (pic_for_image image) 
                            win 
                            image_construct_events

data PicWithBGAttr = PicWithBGAttr 
    { with_attr_pic :: Picture
    , with_attr_win :: DebugWindow
    , with_attr_construct_log :: ImageConstructLog
    , with_attr_specified_attr :: Attr
    } deriving ( Show )

instance Arbitrary PicWithBGAttr where
    arbitrary = do
        DefaultImage image image_construct_events <- arbitrary
        let win = DebugWindow (image_width image) (image_height image)
        attr <- arbitrary
        return $ PicWithBGAttr (pic_for_image image) 
                               win 
                               image_construct_events
                               attr
        
