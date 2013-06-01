-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Graphics.Vty.Image ( DisplayText
                          , Image
                          , image_width
                          , image_height
                          , horiz_join
                          , (<|>)
                          , vert_join
                          , (<->)
                          , horiz_cat
                          , vert_cat
                          , background_fill
                          , text
                          , strict_text
                          , char
                          , string
                          , iso_10646_string
                          , utf8_string
                          , utf8_bytestring
                          , utf8_strict_bytestring
                          , char_fill
                          , empty_image
                          , safe_wcwidth
                          , safe_wcswidth
                          , wcwidth
                          , wcswidth
                          , crop
                          , crop_right
                          , crop_left
                          , crop_bottom
                          , crop_top
                          , pad
                          , resize
                          , translate
                          -- | The possible display attributes used in constructing an `Image`.
                          , module Graphics.Vty.Attributes
                          )
    where

import Graphics.Vty.Attributes
import Graphics.Vty.Image.Internal
import Graphics.Text.Width

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Word

infixr 5 <|>
infixr 4 <->

-- | Currently append in the Monoid instance is equivalent to <->. 
instance Monoid Image where
    mempty = EmptyImage
    mappend = (<->)

-- | combines two images side by side
--
-- Combines text chunks where possible. Assures output_width and output_height properties are not
-- violated.
--
-- The result image will have a width equal to the sum of the two images width.  And the height will
-- equal the largest height of the two images.  The area not defined in one image due to a height
-- missmatch will be filled with the background pattern.
--
-- TODO: the bg fill is biased towards top to bottom languages(?)
horiz_join :: Image -> Image -> Image
horiz_join EmptyImage i          = i
horiz_join i          EmptyImage = i
horiz_join i_0@(HorizText a_0 t_0 w_0 cw_0) i_1@(HorizText a_1 t_1 w_1 cw_1)
    | a_0 == a_1 = HorizText a_0 (TL.append t_0 t_1) (w_0 + w_1) (cw_0 + cw_1)
    -- TODO: assumes horiz text height is always 1
    | otherwise  = HorizJoin i_0 i_1 (w_0 + w_1) 1
horiz_join i_0 i_1
    -- If the images are of the same height then no padding is required
    | h_0 == h_1 = HorizJoin i_0 i_1 w h_0
    -- otherwise one of the images needs to be padded to the right size.
    | h_0 < h_1  -- Pad i_0
        = let pad_amount = h_1 - h_0
          in HorizJoin (VertJoin i_0 (BGFill w_0 pad_amount) w_0 h_1) i_1 w h_1
    | h_0 > h_1  -- Pad i_1
        = let pad_amount = h_0 - h_1
          in HorizJoin i_0 (VertJoin i_1 (BGFill w_1 pad_amount) w_1 h_0) w h_0
    where
        w_0 = image_width i_0
        w_1 = image_width i_1
        w   = w_0 + w_1
        h_0 = image_height i_0
        h_1 = image_height i_1
horiz_join _ _ = error "horiz_join applied to undefined values."

-- | combines two images vertically
--
-- The result image will have a height equal to the sum of the heights of both images.
-- The width will equal the largest width of the two images.
-- The area not defined in one image due to a width missmatch will be filled with the background
-- pattern.
--
-- TODO: the bg fill is biased towards right to left languages(?)
vert_join :: Image -> Image -> Image
vert_join EmptyImage i          = i
vert_join i          EmptyImage = i
vert_join i_0 i_1
    -- If the images are of the same width then no background padding is required
    | w_0 == w_1 = VertJoin i_0 i_1 w_0 h
    -- Otherwise one of the images needs to be padded to the size of the other image.
    | w_0 < w_1
        = let pad_amount = w_1 - w_0
          in VertJoin (HorizJoin i_0 (BGFill pad_amount h_0) w_1 h_0) i_1 w_1 h
    | w_0 > w_1
        = let pad_amount = w_0 - w_1
          in VertJoin i_0 (HorizJoin i_1 (BGFill pad_amount h_1) w_0 h_1) w_0 h
    where
        w_0 = image_width i_0
        w_1 = image_width i_1
        h_0 = image_height i_0
        h_1 = image_height i_1
        h   = h_0 + h_1
vert_join _ _ = error "vert_join applied to undefined values."

-- | An area of the picture's bacground (See Background) of w columns and h rows.
background_fill :: Int -> Int -> Image
background_fill w h 
    | w == 0    = EmptyImage
    | h == 0    = EmptyImage
    | otherwise = BGFill w h

-- | Combines two images horizontally. Alias for horiz_join
--
-- infixr 5
(<|>) :: Image -> Image -> Image
(<|>) = horiz_join

-- | Combines two images vertically. Alias for vert_join
--
-- infixr 4
(<->) :: Image -> Image -> Image
(<->) = vert_join

-- | Compose any number of images horizontally.
horiz_cat :: [Image] -> Image
horiz_cat = foldr (<|>) EmptyImage

-- | Compose any number of images vertically.
vert_cat :: [Image] -> Image
vert_cat = foldr (<->) EmptyImage

-- | A Data.Text.Lazy value
text :: Attr -> TL.Text -> Image
text a txt
    | TL.length txt == 0 = EmptyImage
    | otherwise          = let display_width = safe_wcswidth (TL.unpack txt)
                           in HorizText a txt display_width (fromIntegral $! TL.length txt)

-- | A Data.Text value
strict_text :: Attr -> T.Text -> Image
strict_text a txt
    | T.length txt == 0 = EmptyImage
    | otherwise         = let display_width = safe_wcswidth (T.unpack txt)
                          in HorizText a (TL.fromStrict txt) display_width (T.length txt)

-- | an image of a single character. This is a standard Haskell 31-bit character assumed to be in
-- the ISO-10646 encoding.
char :: Attr -> Char -> Image
char a c = 
    let display_width = safe_wcwidth c
    in HorizText a (TL.singleton c) display_width 1

-- | A string of characters layed out on a single row with the same display attribute. The string is
-- assumed to be a sequence of ISO-10646 characters. 
--
-- Note: depending on how the Haskell compiler represents string literals a string literal in a
-- UTF-8 encoded source file, for example, may be represented as a ISO-10646 string. 
-- That is, I think, the case with GHC 6.10. This means, for the most part, you don't need to worry
-- about the encoding format when outputting string literals. Just provide the string literal
-- directly to iso_10646_string or string.
-- 
iso_10646_string :: Attr -> String -> Image
iso_10646_string a str = 
    let display_width = safe_wcswidth str
    in HorizText a (TL.pack str) display_width (length str)

-- | Alias for iso_10646_string. Since the usual case is that a literal string like "foo" is
-- represented internally as a list of ISO 10646 31 bit characters.  
--
-- Note: Keep in mind that GHC will compile source encoded as UTF-8 but the literal strings, while
-- UTF-8 encoded in the source, will be transcoded to a ISO 10646 31 bit characters runtime
-- representation.
string :: Attr -> String -> Image
string = iso_10646_string

-- | A string of characters layed out on a single row. The input is assumed to be the bytes for
-- UTF-8 encoded text.
utf8_string :: Attr -> [Word8] -> Image
utf8_string a bytes = utf8_bytestring a (BL.pack bytes)

-- | Renders a UTF-8 encoded lazy bytestring. 
utf8_bytestring :: Attr -> BL.ByteString -> Image
utf8_bytestring a bs = text a (TL.decodeUtf8 bs)

-- | Renders a UTF-8 encoded strict bytestring. 
utf8_strict_bytestring :: Attr -> B.ByteString -> Image
utf8_strict_bytestring a bs = strict_text a (T.decodeUtf8 bs)

-- | creates a fill of the specified character. The dimensions are in number of characters wide and
-- number of rows high.
--
-- Unlike the Background fill character this character can have double column display width.
char_fill :: Integral d => Attr -> Char -> d -> d -> Image
char_fill _a _c 0  _h = EmptyImage
char_fill _a _c _w 0  = EmptyImage
char_fill a c w h =
    vert_cat $ replicate (fromIntegral h) $ HorizText a txt display_width char_width
    where 
        txt = TL.replicate (fromIntegral w) (TL.singleton c)
        display_width = safe_wcwidth c * (fromIntegral w)
        char_width = fromIntegral w

-- | The empty image. Useful for fold combinators. These occupy no space nor define any display
-- attributes.
empty_image :: Image 
empty_image = EmptyImage

-- | pad the given image. This adds background character fills to the left, top, right, bottom.
pad :: Int -> Int -> Int -> Int -> Image -> Image
pad 0 0 0 0 i = i
pad in_l in_t in_r in_b in_image
    | in_l < 0 || in_t < 0 || in_r < 0 || in_b < 0 = error "cannot pad by negative amount"
    | otherwise = go in_l in_t in_r in_b in_image
        where 
            -- TODO: uh.
            go 0 0 0 0 i = i
            go 0 0 0 b i = VertJoin i (BGFill w b) w h
                where w = image_width  i
                      h = image_height i + b
            go 0 0 r b i = go 0 0 0 b $ HorizJoin i (BGFill r h) w h
                where w = image_width  i + r
                      h = image_height i
            go 0 t r b i = go 0 0 r b $ VertJoin (BGFill w t) i w h
                where w = image_width  i
                      h = image_height i + t
            go l t r b i = go 0 t r b $ HorizJoin (BGFill l h) i w h
                where w = image_width  i + l
                      h = image_height i

-- | "translates" an image by padding the top and left.
translate :: Int -> Int -> Image -> Image
translate x y i = pad x y 0 0 i

-- | Ensure an image is no larger than the provided size. If the image is larger then crop the right
-- or bottom.
--
-- This is transformed to a vertical crop from the bottom followed by horizontal crop from the
-- right.
crop :: Int -> Int -> Image -> Image
crop 0 _ _ = EmptyImage
crop _ 0 _ = EmptyImage
crop w h i = crop_bottom h (crop_right w i)

-- | crop the display height. If the image is less than or equal in height then this operation has
-- no effect. Otherwise the image is cropped from the bottom.
crop_bottom :: Int -> Image -> Image
crop_bottom 0 _ = EmptyImage
crop_bottom h in_i
    | h < 0     = error "cannot crop height to less than zero"
    | otherwise = go in_i
        where
            go EmptyImage = EmptyImage
            go (CropBottom {cropped_image, output_width, output_height}) =
                CropBottom cropped_image output_width (min h output_height)
            go i
                | h >= image_height i = i
                | otherwise           = CropBottom i (image_width i) h

crop_right :: Int -> Image -> Image
crop_right 0 _ = EmptyImage
crop_right w in_i
    | w < 0     = error "cannot crop width to less than zero"
    | otherwise = go in_i
        where
            go EmptyImage = EmptyImage
            go (CropRight {cropped_image, output_width, output_height}) =
                CropRight cropped_image (min w output_width) output_height
            go i
                | w >= image_width i = i
                | otherwise          = CropRight i w (image_height i)

crop_left :: Int -> Image -> Image
crop_left _ _ = error "not implemented"

crop_top :: Int -> Image -> Image
crop_top _ _ = error "not implemented"

-- | Generic resize. Pads and crops as required to assure the given display width and height.
-- This is biased to pad the right and bottom.
resize :: Int -> Int -> Image -> Image
resize _w _h _i = error "not implemented yet"

