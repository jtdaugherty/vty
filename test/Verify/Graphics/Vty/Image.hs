module Verify.Graphics.Vty.Image ( module Verify.Graphics.Vty.Image
                                 , module Graphics.Vty.Image
                                 )
    where

import Graphics.Vty.Debug
import Graphics.Vty.Image
import Graphics.Vty.WinRegion

import Verify
import Verify.Graphics.Vty.Attributes

import Data.Word

data UnitImage = UnitImage Char Image

instance Arbitrary UnitImage where
    arbitrary = do
        SingleColumnChar c <- arbitrary
        return $ UnitImage c (char def_attr c)

instance Show UnitImage where
    show (UnitImage c _) = "UnitImage " ++ show c

data DefaultImage = DefaultImage Image ImageConstructLog

instance Show DefaultImage where
    show (DefaultImage i image_log) 
        = "DefaultImage (" ++ show i ++ ") " ++ show (image_width i, image_height i) ++ " " ++ show image_log

instance Arbitrary DefaultImage where
    arbitrary = do
        i <- return $ char def_attr 'X' -- elements forward_image_ops >>= return . (\op -> op empty_image)
        return $ DefaultImage i []

data SingleRowSingleAttrImage = SingleRowSingleAttrImage Attr Word Image

instance Show SingleRowSingleAttrImage where
    show (SingleRowSingleAttrImage attr columns image) 
        = "SingleRowSingleAttrImage (" ++ show attr ++ ") " ++ show columns ++ " ( " ++ show image ++ " )"

instance Arbitrary SingleRowSingleAttrImage where
    arbitrary = do
        single_column_row_text <- arbitrary
        attr <- arbitrary
        return $ SingleRowSingleAttrImage 
                    attr 
                    ( fromIntegral $ length single_column_row_text )
                    ( horiz_cat $ [ char attr c | SingleColumnChar c <- single_column_row_text ] )


data SingleAttrSingleSpanStack = SingleAttrSingleSpanStack 
    { stack_image :: Image 
    , stack_source_images :: [SingleRowSingleAttrImage]
    , stack_width :: Word
    , stack_height :: Word
    }
    deriving Show

instance Arbitrary SingleAttrSingleSpanStack where
    arbitrary = do
        NonEmpty image_list <- arbitrary
        let image = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- image_list ]
        return $ SingleAttrSingleSpanStack 
                    image 
                    image_list 
                    ( maximum $ map expected_columns image_list )
                    ( toEnum $ length image_list )

