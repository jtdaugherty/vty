{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Verify.Graphics.Vty.Image ( module Verify.Graphics.Vty.Image
                                 , module Graphics.Vty.Image
                                 )
    where

import Verify.Graphics.Vty.Attributes
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

data DefaultImage = DefaultImage Image

instance Show DefaultImage where
    show (DefaultImage i) 
        = "DefaultImage (" ++ show i ++ ") " ++ show (image_width i, image_height i)

instance Arbitrary DefaultImage where
    arbitrary = do
        i <- return $ char def_attr 'X'
        return $ DefaultImage i

data SingleRowSingleAttrImage 
    = SingleRowSingleAttrImage 
      { expected_attr :: Attr
      , expected_columns :: Int
      , row_image :: Image
      }

instance Show SingleRowSingleAttrImage where
    show (SingleRowSingleAttrImage attr columns image) 
        = "SingleRowSingleAttrImage (" ++ show attr ++ ") " ++ show columns ++ " ( " ++ show image ++ " )"

newtype WidthResize = WidthResize (Image -> (Image, Int))

instance Arbitrary WidthResize where
    arbitrary = oneof
        [ return $ WidthResize $ \i -> (i, image_width i)
        , do
            WidthResize f <- arbitrary
            out_width <- choose (1, 256)
            return $ WidthResize $ \i -> (resize_width out_width $ fst $ f i, out_width)
        ]

newtype HeightResize = HeightResize (Image -> (Image, Int))

instance Arbitrary HeightResize where
    arbitrary = oneof
        [ return $ HeightResize $ \i -> (i, image_height i)
        , do
            HeightResize f <- arbitrary
            out_height <- choose (1, 256)
            return $ HeightResize $ \i -> (resize_height out_height $ fst $ f i, out_height)
        ]

newtype ImageResize = ImageResize (Image -> (Image, (Int, Int)))

instance Arbitrary ImageResize where
    arbitrary = oneof
        [ return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , return $! ImageResize $! \i -> (i, (image_width i, image_height i))
        , do
            ImageResize f <- arbitrary
            WidthResize g <- arbitrary
            return $! ImageResize $! \i -> 
                let (i_0, (_, out_height)) = f i
                    g_i = g i_0
                in (fst g_i, (snd g_i, out_height))
        , do
            ImageResize f <- arbitrary
            HeightResize g <- arbitrary
            return $! ImageResize $! \i -> 
                let (i_0, (out_width, _)) = f i
                    g_i = g i_0
                in (fst g_i, (out_width, snd g_i))
        ]


instance Arbitrary SingleRowSingleAttrImage where
    arbitrary = do
        -- The text must contain at least one character. Otherwise the image simplifies to the
        -- IdImage which has a height of 0. If this is to represent a single row then the height
        -- must be 1
        single_column_row_text <- Verify.resize 128 (listOf1 arbitrary)
        a <- arbitrary
        let out_image = horiz_cat $ [char a c | SingleColumnChar c <- single_column_row_text]
            out_width = length single_column_row_text
        return $ SingleRowSingleAttrImage a out_width out_image

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

instance Arbitrary Image  where
    arbitrary = oneof
        [ do
            SingleAttrSingleSpanStack {stack_image} <- arbitrary
            ImageResize f <- arbitrary
            return $! fst $! f stack_image
        , do
            SingleAttrSingleSpanStack {stack_image} <- arbitrary
            ImageResize f <- arbitrary
            return $! fst $! f stack_image
        , do
            SingleAttrSingleSpanStack {stack_image} <- arbitrary
            ImageResize f <- arbitrary
            return $! fst $! f stack_image
        , do
            i_0 <- arbitrary
            i_1 <- arbitrary
            let i = i_0 <|> i_1
            ImageResize f <- arbitrary
            return $! fst $! f i
        , do
            i_0 <- arbitrary
            i_1 <- arbitrary
            let i = i_0 <-> i_1
            ImageResize f <- arbitrary
            return $! fst $! f i
        ]

