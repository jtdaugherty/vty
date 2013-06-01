{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Verify.Graphics.Vty.Image ( module Verify.Graphics.Vty.Image
                                 , module Graphics.Vty.Image
                                 )
    where

import Verify.Graphics.Vty.Attributes
import Graphics.Vty.Debug.Image
import Graphics.Vty.Image

import Verify

data UnitImage = UnitImage Char Image

instance Arbitrary UnitImage where
    arbitrary = do
        SingleColumnChar c <- arbitrary
        a <- arbitrary
        return $ UnitImage c (char a c)

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

data SingleRowSingleAttrImage 
    = SingleRowSingleAttrImage 
      { expected_attr :: Attr
      , expected_columns :: Int
      , row_image :: Image
      }

instance Show SingleRowSingleAttrImage where
    show (SingleRowSingleAttrImage attr columns image) 
        = "SingleRowSingleAttrImage (" ++ show attr ++ ") " ++ show columns ++ " ( " ++ show image ++ " )"

instance Arbitrary SingleRowSingleAttrImage where
    arbitrary = do
        -- The text must contain at least one character. Otherwise the image simplifies to the
        -- IdImage which has a height of 0. If this is to represent a single row then the height
        -- must be 1
        single_column_row_text <- Verify.resize 128 (listOf1 arbitrary)
        a <- arbitrary
        return $ SingleRowSingleAttrImage 
                    a
                    ( fromIntegral $ length single_column_row_text )
                    ( horiz_cat $ [ char a c | SingleColumnChar c <- single_column_row_text ] )

data SingleRowTwoAttrImage 
    = SingleRowTwoAttrImage 
    { part_0 :: SingleRowSingleAttrImage
    , part_1 :: SingleRowSingleAttrImage
    , join_image :: Image
    } deriving Show

instance Arbitrary SingleRowTwoAttrImage where
    arbitrary = do
        p0 <- arbitrary
        p1 <- arbitrary
        return $ SingleRowTwoAttrImage p0 p1 (row_image p0 <|> row_image p1)

data SingleAttrSingleSpanStack = SingleAttrSingleSpanStack 
    { stack_image :: Image 
    , stack_source_images :: [SingleRowSingleAttrImage]
    , stack_width :: Int
    , stack_height :: Int
    }
    deriving Show

instance Arbitrary SingleAttrSingleSpanStack where
    arbitrary = do
        image_list <- Verify.resize 128 (listOf1 arbitrary)
        let image = vert_cat [ i | SingleRowSingleAttrImage { row_image = i } <- image_list ]
        return $ SingleAttrSingleSpanStack 
                    image 
                    image_list 
                    ( maximum $ map expected_columns image_list )
                    ( toEnum $ length image_list )

