{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Verify.Graphics.Vty.Image
  ( module Verify.Graphics.Vty.Image
  , module Graphics.Vty.Image
  )
where

import Verify.Graphics.Vty.Attributes
import Graphics.Vty.Image
import Graphics.Vty.Image.Internal

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
        = "DefaultImage (" ++ show i ++ ") " ++ show (imageWidth i, imageHeight i)

instance Arbitrary DefaultImage where
    arbitrary = do
        i <- return $ char defAttr 'X'
        return $ DefaultImage i

data SingleRowSingleAttrImage
    = SingleRowSingleAttrImage
      { expectedAttr :: Attr
      , expectedColumns :: Int
      , rowImage :: Image
      }

instance Show SingleRowSingleAttrImage where
    show (SingleRowSingleAttrImage attr columns image)
        = "SingleRowSingleAttrImage (" ++ show attr ++ ") " ++ show columns ++ " ( " ++ show image ++ " )"

newtype WidthResize = WidthResize (Image -> (Image, Int))

instance Arbitrary WidthResize where
    arbitrary = do
        WidthResize f <- arbitrary
        w <- choose (1,64)
        oneof $ map (return . WidthResize)
            [ \i -> (i, imageWidth i)
            , \i -> (resizeWidth w $ fst $ f i, w)
            , \i -> let i' = fst $ f i in (cropLeft w i', min (imageWidth i') w)
            , \i -> let i' = fst $ f i in (cropRight w i', min (imageWidth i') w)
            ]

newtype HeightResize = HeightResize (Image -> (Image, Int))

instance Arbitrary HeightResize where
    arbitrary = do
        HeightResize f <- arbitrary
        h <- choose (1,64)
        oneof $ map (return . HeightResize)
            [ \i -> (i, imageHeight i)
            , \i -> (resizeHeight h $ fst $ f i, h)
            , \i -> let i' = fst $ f i in (cropTop h i', min (imageHeight i') h)
            , \i -> let i' = fst $ f i in (cropBottom h i', min (imageHeight i') h)
            ]

newtype ImageResize = ImageResize (Image -> (Image, (Int, Int)))

instance Arbitrary ImageResize where
    arbitrary = oneof
        [ return $! ImageResize $! \i -> (i, (imageWidth i, imageHeight i))
        , return $! ImageResize $! \i -> (i, (imageWidth i, imageHeight i))
        , return $! ImageResize $! \i -> (i, (imageWidth i, imageHeight i))
        , return $! ImageResize $! \i -> (i, (imageWidth i, imageHeight i))
        , return $! ImageResize $! \i -> (i, (imageWidth i, imageHeight i))
        , return $! ImageResize $! \i -> (i, (imageWidth i, imageHeight i))
        , do
            ImageResize f <- arbitrary
            WidthResize g <- arbitrary
            return $! ImageResize $! \i ->
                let (i0, (_, outHeight)) = f i
                    gI = g i0
                in (fst gI, (snd gI, outHeight))
        , do
            ImageResize f <- arbitrary
            HeightResize g <- arbitrary
            return $! ImageResize $! \i ->
                let (i0, (outWidth, _)) = f i
                    gI = g i0
                in (fst gI, (outWidth, snd gI))
        ]


instance Arbitrary SingleRowSingleAttrImage where
    arbitrary = do
        -- The text must contain at least one character. Otherwise the
        -- image simplifies to the IdImage which has a height of 0. If
        -- this is to represent a single row then the height must be 1
        singleColumnRowText <- Verify.resize 16 (listOf1 arbitrary)
        a <- arbitrary
        let outImage = horizCat $ [char a c | SingleColumnChar c <- singleColumnRowText]
            outWidth = length singleColumnRowText
        return $ SingleRowSingleAttrImage a outWidth outImage

data SingleRowTwoAttrImage
    = SingleRowTwoAttrImage
    { part0 :: SingleRowSingleAttrImage
    , part1 :: SingleRowSingleAttrImage
    , joinImage :: Image
    } deriving Show

instance Arbitrary SingleRowTwoAttrImage where
    arbitrary = do
        p0 <- arbitrary
        p1 <- arbitrary
        return $ SingleRowTwoAttrImage p0 p1 (rowImage p0 <|> rowImage p1)

data SingleAttrSingleSpanStack = SingleAttrSingleSpanStack
    { stackImage :: Image
    , stackSourceImages :: [SingleRowSingleAttrImage]
    , stackWidth :: Int
    , stackHeight :: Int
    }
    deriving Show

instance Arbitrary SingleAttrSingleSpanStack where
    arbitrary = do
        imageList <- Verify.resize 16 (listOf1 arbitrary)
        return $ mkSingleAttrSingleSpanStack imageList
    shrink s = do
        imageList <- shrink $ stackSourceImages s
        if null imageList
            then []
            else return $ mkSingleAttrSingleSpanStack imageList

mkSingleAttrSingleSpanStack imageList =
    let image = vertCat [ i | SingleRowSingleAttrImage { rowImage = i } <- imageList ]
    in SingleAttrSingleSpanStack image imageList (maximum $ map expectedColumns imageList)
                                                 (toEnum $ length imageList)

instance Arbitrary Image  where
    arbitrary = oneof
        [ return EmptyImage
        , do
            SingleAttrSingleSpanStack {stackImage} <- Verify.resize 8 arbitrary
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f stackImage
        , do
            SingleAttrSingleSpanStack {stackImage} <- Verify.resize 8 arbitrary
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f stackImage
        , do
            i0 <- arbitrary
            i1 <- arbitrary
            let i = i0 <|> i1
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f i
        , do
            i0 <- arbitrary
            i1 <- arbitrary
            let i = i0 <-> i1
            ImageResize f <- Verify.resize 2 arbitrary
            return $! fst $! f i
        ]
    {-
    shrink i@(HorizJoin {partLeft, partRight}) = do
        let !i_alt = backgroundFill (imageWidth i) (imageHeight i)
        !partLeft' <- shrink partLeft
        !partRight' <- shrink partRight
        [i_alt, partLeft' <|> partRight']
    shrink i@(VertJoin {partTop, partBottom}) = do
        let !i_alt = backgroundFill (imageWidth i) (imageHeight i)
        !partTop' <- shrink partTop
        !partBottom' <- shrink partBottom
        [i_alt, partTop' <-> partBottom']
    shrink i = [emptyImage, backgroundFill (imageWidth i) (imageHeight i)]
    -}

data CropOperation
    = CropFromLeft
    | CropFromRight
    | CropFromTop
    | CropFromBottom
    deriving (Eq, Show)

instance Arbitrary CropOperation where
    arbitrary = oneof $ map return [CropFromLeft, CropFromRight, CropFromTop, CropFromBottom]

data Translation = Translation Image (Int, Int) Image
    deriving (Eq, Show)

instance Arbitrary Translation where
    arbitrary = do
        i <- arbitrary
        x <- arbitrary `suchThat` (> 0)
        y <- arbitrary `suchThat` (> 0)
        let i' = translate x y i
        return $ Translation i (x,y) i'
