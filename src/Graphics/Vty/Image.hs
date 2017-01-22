-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Graphics.Vty.Image
  ( DisplayText
  , Image
  , imageWidth
  , imageHeight
  , horizJoin
  , (<|>)
  , vertJoin
  , (<->)
  , horizCat
  , vertCat
  , backgroundFill
  , text
  , text'
  , char
  , string
  , iso10646String
  , utf8String
  , utf8Bytestring
  , utf8Bytestring'
  , charFill
  , emptyImage
  , safeWcwidth
  , safeWcswidth
  , wcwidth
  , wcswidth
  , crop
  , cropRight
  , cropLeft
  , cropBottom
  , cropTop
  , pad
  , resize
  , resizeWidth
  , resizeHeight
  , translate
  , translateX
  , translateY
  -- | The possible display attributes used in
  -- constructing an `Image`.
  , module Graphics.Vty.Attributes
  )
where

import Graphics.Vty.Attributes
import Graphics.Vty.Image.Internal
import Graphics.Text.Width

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word

infixr 5 <|>
infixr 4 <->

-- | An area of the picture's bacground (See Background) of w columns
-- and h rows.
backgroundFill :: Int -> Int -> Image
backgroundFill w h
    | w == 0    = EmptyImage
    | h == 0    = EmptyImage
    | otherwise = BGFill w h

-- | Combines two images horizontally. Alias for horizJoin
--
-- infixr 5
(<|>) :: Image -> Image -> Image
(<|>) = horizJoin

-- | Combines two images vertically. Alias for vertJoin
--
-- infixr 4
(<->) :: Image -> Image -> Image
(<->) = vertJoin

-- | Compose any number of images horizontally.
horizCat :: [Image] -> Image
horizCat = foldr horizJoin EmptyImage

-- | Compose any number of images vertically.
vertCat :: [Image] -> Image
vertCat = foldr vertJoin EmptyImage

-- | A Data.Text.Lazy value
text :: Attr -> TL.Text -> Image
text a txt = let displayWidth = safeWcswidth (TL.unpack txt)
             in HorizText a txt displayWidth (fromIntegral $! TL.length txt)

-- | A Data.Text value
text' :: Attr -> T.Text -> Image
text' a txt = let displayWidth = safeWcswidth (T.unpack txt)
              in HorizText a (TL.fromStrict txt) displayWidth (T.length txt)

-- | An image of a single character. This is a standard Haskell 31-bit
-- character assumed to be in the ISO-10646 encoding.
char :: Attr -> Char -> Image
char a c =
    let displayWidth = safeWcwidth c
    in HorizText a (TL.singleton c) displayWidth 1

-- | A string of characters layed out on a single row with the same
-- display attribute. The string is assumed to be a sequence of
-- ISO-10646 characters.
--
-- Note: depending on how the Haskell compiler represents string
-- literals a string literal in a UTF-8 encoded source file, for
-- example, may be represented as a ISO-10646 string. That is, I think,
-- the case with GHC 6.10. This means, for the most part, you don't need
-- to worry about the encoding format when outputting string literals.
-- Just provide the string literal directly to iso10646String or string.
iso10646String :: Attr -> String -> Image
iso10646String a str =
    let displayWidth = safeWcswidth str
    in HorizText a (TL.pack str) displayWidth (length str)

-- | Alias for iso10646String. Since the usual case is that a literal
-- string like "foo" is represented internally as a list of ISO 10646 31
-- bit characters.
--
-- Note: Keep in mind that GHC will compile source encoded as UTF-8
-- but the literal strings, while UTF-8 encoded in the source, will be
-- transcoded to a ISO 10646 31 bit characters runtime representation.
string :: Attr -> String -> Image
string = iso10646String

-- | A string of characters layed out on a single row. The input is
-- assumed to be the bytes for UTF-8 encoded text.
utf8String :: Attr -> [Word8] -> Image
utf8String a bytes = utf8Bytestring a (BL.pack bytes)

-- | Renders a UTF-8 encoded lazy bytestring.
utf8Bytestring :: Attr -> BL.ByteString -> Image
utf8Bytestring a bs = text a (TL.decodeUtf8 bs)

-- | Renders a UTF-8 encoded strict bytestring.
utf8Bytestring' :: Attr -> B.ByteString -> Image
utf8Bytestring' a bs = text' a (T.decodeUtf8 bs)

-- | Creates a fill of the specified character. The dimensions are in
-- number of characters wide and number of rows high.
charFill :: Integral d => Attr -> Char -> d -> d -> Image
charFill _a _c 0  _h = EmptyImage
charFill _a _c _w 0  = EmptyImage
charFill a c w h =
    vertCat $ replicate (fromIntegral h) $ HorizText a txt displayWidth charWidth
    where
        txt = TL.replicate (fromIntegral w) (TL.singleton c)
        displayWidth = safeWcwidth c * (fromIntegral w)
        charWidth = fromIntegral w

-- | The empty image. Useful for fold combinators. These occupy no space
-- nor define any display attributes.
emptyImage :: Image
emptyImage = EmptyImage

-- | Pad the given image. This adds background character fills to the
-- left, top, right, bottom. The pad values are how many display columns
-- or rows to add.
pad :: Int -> Int -> Int -> Int -> Image -> Image
pad 0 0 0 0 i = i
pad inL inT inR inB inImage
    | inL < 0 || inT < 0 || inR < 0 || inB < 0 = error "cannot pad by negative amount"
    | otherwise = go inL inT inR inB inImage
        where
            -- TODO: uh.
            go 0 0 0 0 i = i
            go 0 0 0 b i = VertJoin i (BGFill w b) w h
                where w = imageWidth  i
                      h = imageHeight i + b
            go 0 0 r b i = go 0 0 0 b $ HorizJoin i (BGFill r h) w h
                where w = imageWidth  i + r
                      h = imageHeight i
            go 0 t r b i = go 0 0 r b $ VertJoin (BGFill w t) i w h
                where w = imageWidth  i
                      h = imageHeight i + t
            go l t r b i = go 0 t r b $ HorizJoin (BGFill l h) i w h
                where w = imageWidth  i + l
                      h = imageHeight i

-- | Translates an image by padding or cropping the left and top.
-- First param is amount to translate left. Second param is amount to
-- translate top.
--
-- This can have an unexpected effect: Translating an image to less than
-- (0,0) then to greater than (0,0) will crop the image.
translate :: Int -> Int -> Image -> Image
translate x y i = translateX x (translateY y i)

-- | translates an image by padding or cropping the left
translateX :: Int -> Image -> Image
translateX x i
    | x < 0     = let s = abs x in CropLeft i s (imageWidth i - s) (imageHeight i)
    | x == 0    = i
    | otherwise = let h = imageHeight i in HorizJoin (BGFill x h) i (imageWidth i + x) h

-- | translates an image by padding or cropping the top
translateY :: Int -> Image -> Image
translateY y i
    | y < 0     = let s = abs y in CropTop i s (imageWidth i) (imageHeight i - s)
    | y == 0    = i
    | otherwise = let w = imageWidth i in VertJoin (BGFill w y) i w (imageHeight i + y)

-- | Ensure an image is no larger than the provided size. If the image
-- is larger then crop the right or bottom.
--
-- This is transformed to a vertical crop from the bottom followed by
-- horizontal crop from the right.
crop :: Int -> Int -> Image -> Image
crop 0 _ _ = EmptyImage
crop _ 0 _ = EmptyImage
crop w h i = cropBottom h (cropRight w i)

-- | crop the display height. If the image is less than or equal in
-- height then this operation has no effect. Otherwise the image is
-- cropped from the bottom.
cropBottom :: Int -> Image -> Image
cropBottom 0 _ = EmptyImage
cropBottom h inI
    | h < 0     = error "cannot crop height to less than zero"
    | otherwise = go inI
        where
            go EmptyImage = EmptyImage
            go i@(CropBottom {croppedImage, outputWidth, outputHeight})
                | outputHeight <= h = i
                | otherwise          = CropBottom croppedImage outputWidth h
            go i
                | h >= imageHeight i = i
                | otherwise           = CropBottom i (imageWidth i) h

-- | ensure the image is no wider than the given width. If the image is
-- wider then crop the right side.
cropRight :: Int -> Image -> Image
cropRight 0 _ = EmptyImage
cropRight w inI
    | w < 0     = error "cannot crop width to less than zero"
    | otherwise = go inI
        where
            go EmptyImage = EmptyImage
            go i@(CropRight {croppedImage, outputWidth, outputHeight})
                | outputWidth <= w = i
                | otherwise         = CropRight croppedImage w outputHeight
            go i
                | w >= imageWidth i = i
                | otherwise          = CropRight i w (imageHeight i)

-- | ensure the image is no wider than the given width. If the image is
-- wider then crop the left side.
cropLeft :: Int -> Image -> Image
cropLeft 0 _ = EmptyImage
cropLeft w inI
    | w < 0     = error "cannot crop the width to less than zero"
    | otherwise = go inI
        where
            go EmptyImage = EmptyImage
            go i@(CropLeft {croppedImage, leftSkip, outputWidth, outputHeight})
                | outputWidth <= w = i
                | otherwise         =
                    let leftSkip' = leftSkip + outputWidth - w
                    in CropLeft croppedImage leftSkip' w outputHeight
            go i
                | imageWidth i <= w = i
                | otherwise          = CropLeft i (imageWidth i - w) w (imageHeight i)

-- | crop the display height. If the image is less than or equal in
-- height then this operation has no effect. Otherwise the image is
-- cropped from the top.
cropTop :: Int -> Image -> Image
cropTop 0 _ = EmptyImage
cropTop h inI
    | h < 0  = error "cannot crop the height to less than zero"
    | otherwise = go inI
        where
            go EmptyImage = EmptyImage
            go i@(CropTop {croppedImage, topSkip, outputWidth, outputHeight})
                | outputHeight <= h = i
                | otherwise         =
                    let topSkip' = topSkip + outputHeight - h
                    in CropTop croppedImage topSkip' outputWidth h
            go i
                | imageHeight i <= h = i
                | otherwise          = CropTop i (imageHeight i - h) (imageWidth i) h

-- | Generic resize. Pads and crops as required to assure the given
-- display width and height. This is biased to pad/crop the right and
-- bottom.
resize :: Int -> Int -> Image -> Image
resize w h i = resizeHeight h (resizeWidth w i)

-- | Resize the width. Pads and crops as required to assure the given
-- display width. This is biased to pad/crop the right.
resizeWidth :: Int -> Image -> Image
resizeWidth w i = case w `compare` imageWidth i of
    LT -> cropRight w i
    EQ -> i
    GT -> i <|> BGFill (w - imageWidth i) (imageHeight i)

-- | Resize the height. Pads and crops as required to assure the given
-- display height. This is biased to pad/crop the bottom.
resizeHeight :: Int -> Image -> Image
resizeHeight h i = case h `compare` imageHeight i of
    LT -> cropBottom h i
    EQ -> i
    GT -> i <-> BGFill (imageWidth i) (h - imageHeight i)
