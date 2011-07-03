-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Graphics.Vty.Image ( Image(..)
                          , image_width
                          , image_height
                          , (<|>)
                          , (<->)
                          , horiz_cat
                          , vert_cat
                          , background_fill
                          , char
                          , string
                          , iso_10646_string
                          , utf8_string
                          , utf8_bytestring
                          , char_fill
                          , empty_image
                          , translate
                          , safe_wcwidth
                          , safe_wcswidth
                          , wcwidth
                          , wcswidth
                          , crop
                          , pad
                          -- | The possible display attributes used in constructing an `Image`.
                          , module Graphics.Vty.Attributes
                          )
    where

import Graphics.Vty.Attributes

import Codec.Binary.UTF8.Width

import Codec.Binary.UTF8.String ( decode )

import qualified Data.ByteString as BS
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.String.UTF8 as UTF8
import Data.Word

infixr 5 <|>
infixr 4 <->

-- | We pair each character with it's display length. This way we only compute the length once per
-- character.
-- * Though currently the width of some strings is still compute multiple times. 
type DisplayString = Seq.Seq (Char, Word)

-- | An image in VTY defines:
--
--  * properties required to display the image. These are properties that effect the output image
--    but are independent of position 
--
--  * A set of position-dependent text and attribute regions. The possible regions are:
--
--      * a point. ( char )
--
--      * a horizontal line of characters with a single attribute. (string, utf8_string,
--      utf8_bytestring )
--
--      * a fill of a single character. (char_fill)
--
--      * a fill of the picture's background. (background_fill)
--
-- todo: increase the number of encoded bytestring formats supported.
data Image = 
    -- A horizontal text span is always >= 1 column and has a row height of 1.
      HorizText
      { attr :: !Attr
      -- All character data is stored as Char sequences with the ISO-10646 encoding.
      , text :: DisplayString
      , output_width :: !Word -- >= 0
      , char_width :: !Word -- >= 1
      }
    -- A horizontal join can be constructed between any two images. However a HorizJoin instance is
    -- required to be between two images of equal height. The horiz_join constructor adds background
    -- filles to the provided images that assure this is true for the HorizJoin value produced.
    | HorizJoin
      { part_left :: Image 
      , part_right :: Image
      , output_width :: !Word -- >= 1
      , output_height :: !Word -- >= 1
      }
    -- A veritical join can be constructed between any two images. However a VertJoin instance is
    -- required to be between two images of equal width. The vert_join constructor adds background
    -- fills to the provides images that assure this is true for the VertJoin value produced.
    | VertJoin
      { part_top :: Image
      , part_bottom :: Image
      , output_width :: !Word -- >= 1
      , output_height :: !Word -- >= 1
      }
    -- A background fill will be filled with the background pattern. The background pattern is
    -- defined as a property of the Picture this Image is used to form. 
    | BGFill
      { output_width :: !Word -- >= 1
      , output_height :: !Word -- >= 1
      }
    -- The combining operators identity constant. 
    -- EmptyImage <|> a = a
    -- EmptyImage <-> a = a
    -- 
    -- Any image of zero size equals the empty image.
    | EmptyImage
    | Translation (Int, Int) Image
    -- Crop an image to a size
    | ImageCrop (Word, Word) Image
    -- Pad an image up to a size
    | ImagePad (Word, Word) Image
    deriving Eq

instance Show Image where
    show ( HorizText { output_width = ow, text = txt } ) 
        = "HorizText [" ++ show ow ++ "] (" ++ show (fmap fst txt) ++ ")"
    show ( BGFill { output_width = c, output_height = r } ) 
        = "BGFill (" ++ show c ++ "," ++ show r ++ ")"
    show ( HorizJoin { part_left = l, part_right = r, output_width = c } ) 
        = "HorizJoin " ++ show c ++ " ( " ++ show l ++ " <|> " ++ show r ++ " )"
    show ( VertJoin { part_top = t, part_bottom = b, output_width = c, output_height = r } ) 
        = "VertJoin (" ++ show c ++ ", " ++ show r ++ ") ( " ++ show t ++ " ) <-> ( " ++ show b ++ " )"
    show ( Translation offset i )
        = "Translation " ++ show offset ++ " ( " ++ show i ++ " )"
    show ( ImageCrop size i )
        = "ImageCrop " ++ show size ++ " ( " ++ show i ++ " )"
    show ( ImagePad size i )
        = "ImagePad " ++ show size ++ " ( " ++ show i ++ " )"
    show ( EmptyImage ) = "EmptyImage"

-- | Currently append in the Monoid instance is equivalent to <->. 
instance Monoid Image where
    mempty = empty_image
    mappend = (<->)
    
-- A horizontal text image of 0 characters in width simplifies to the EmptyImage
horiz_text :: Attr -> DisplayString -> Word -> Image
horiz_text a txt ow
    | ow == 0    = EmptyImage
    | otherwise = HorizText a txt ow (toEnum $ Seq.length txt)

horiz_join :: Image -> Image -> Word -> Word -> Image
horiz_join i_0 i_1 w h
    -- A horiz join of two 0 width images simplifies to the EmptyImage
    | w == 0 = EmptyImage
    -- A horizontal join where either part is 0 columns in width simplifies to the other part.
    -- This covers the case where one part is the EmptyImage.
    | image_width i_0 == 0 = i_1
    | image_width i_1 == 0 = i_0
    -- If the images are of the same height then no BG padding is required
    | image_height i_0 == image_height i_1 = HorizJoin i_0 i_1 w h
    -- otherwise one of the imagess needs to be padded to the right size.
    | image_height i_0 < image_height i_1  -- Pad i_0
        = let pad_amount = image_height i_1 - image_height i_0
          in horiz_join ( vert_join i_0 
                                    ( BGFill ( image_width i_0 ) pad_amount ) 
                                    ( image_width i_0 )
                                    ( image_height i_1 )
                        ) 
                        i_1 
                        w h
    | image_height i_0 > image_height i_1  -- Pad i_1
        = let pad_amount = image_height i_0 - image_height i_1
          in horiz_join i_0 
                        ( vert_join i_1 
                                    ( BGFill ( image_width i_1 ) pad_amount ) 
                                    ( image_width i_1 )
                                    ( image_height i_0 )
                        )
                        w h
horiz_join _ _ _ _ = error "horiz_join applied to undefined values."

vert_join :: Image -> Image -> Word -> Word -> Image
vert_join i_0 i_1 w h
    -- A vertical join of two 0 height images simplifies to the EmptyImage
    | h == 0                = EmptyImage
    -- A vertical join where either part is 0 rows in height simplifies to the other part.
    -- This covers the case where one part is the EmptyImage
    | image_height i_0 == 0 = i_1
    | image_height i_1 == 0 = i_0
    -- If the images are of the same height then no background padding is required
    | image_width i_0 == image_width i_1 = VertJoin i_0 i_1 w h
    -- Otherwise one of the images needs to be padded to the size of the other image.
    | image_width i_0 < image_width i_1
        = let pad_amount = image_width i_1 - image_width i_0
          in vert_join ( horiz_join i_0
                                    ( BGFill pad_amount ( image_height i_0 ) )
                                    ( image_width i_1 )
                                    ( image_height i_0 )
                       )
                       i_1
                       w h
    | image_width i_0 > image_width i_1
        = let pad_amount = image_width i_0 - image_width i_1
          in vert_join i_0
                       ( horiz_join i_1
                                    ( BGFill pad_amount ( image_height i_1 ) )
                                    ( image_width i_0 )
                                    ( image_height i_1 )
                       )
                       w h
vert_join _ _ _ _ = error "vert_join applied to undefined values."

-- | An area of the picture's bacground (See Background) of w columns and h rows.
background_fill :: Word -> Word -> Image
background_fill w h 
    | w == 0    = EmptyImage
    | h == 0    = EmptyImage
    | otherwise = BGFill w h

-- | The width of an Image. This is the number display columns the image will occupy.
image_width :: Image -> Word
image_width HorizText { output_width = w } = w
image_width HorizJoin { output_width = w } = w
image_width VertJoin { output_width = w } = w
image_width BGFill { output_width = w } = w
image_width EmptyImage = 0
image_width ( Translation v i ) = toEnum $ max 0 $ (fst v +) $ fromEnum $ image_width i
image_width ( ImageCrop v i ) = min (image_width i) $ fst v
image_width ( ImagePad v i ) = max (image_width i) $ fst v

-- | The height of an Image. This is the number of display rows the image will occupy.
image_height :: Image -> Word
image_height HorizText {} = 1
image_height HorizJoin { output_height = r } = r
image_height VertJoin { output_height = r } = r
image_height BGFill { output_height = r } = r
image_height EmptyImage = 0
image_height ( Translation v i ) = toEnum $ max 0 $ (snd v +) $ fromEnum $ image_height i
image_height ( ImageCrop v i ) = min (image_height i) $ snd v
image_height ( ImagePad v i ) = max (image_height i) $ snd v

-- | Combines two images side by side.
--
-- The result image will have a width equal to the sum of the two images width.  And the height will
-- equal the largest height of the two images.  The area not defined in one image due to a height
-- missmatch will be filled with the background pattern.
(<|>) :: Image -> Image -> Image

-- Two horizontal text spans with the same attributes can be merged.
h0@(HorizText attr_0 text_0 ow_0 _) <|> h1@(HorizText attr_1 text_1 ow_1 _)  
    | attr_0 == attr_1  = horiz_text attr_0 (text_0 Seq.>< text_1) (ow_0 + ow_1)
    | otherwise         = horiz_join h0 h1 (ow_0 + ow_1) 1

-- Anything placed to the right of a join wil be joined to the right sub image.
-- The total columns for the join is the sum of the two arguments columns
h0@( HorizJoin {} ) <|> h1 
    = horiz_join ( part_left h0 ) 
                 ( part_right h0 <|> h1 )
                 ( image_width h0 + image_width h1 )
                 ( max (image_height h0) (image_height h1) )

-- Anything but a join placed to the left of a join wil be joined to the left sub image.
-- The total columns for the join is the sum of the two arguments columns
h0 <|> h1@( HorizJoin {} ) 
    = horiz_join ( h0 <|> part_left h1 ) 
                 ( part_right h1 )
                 ( image_width h0 + image_width h1 )
                 ( max (image_height h0) (image_height h1) )

h0 <|> h1 
    = horiz_join h0 
                 h1 
                 ( image_width h0 + image_width h1 )
                 ( max (image_height h0) (image_height h1) )

-- | Combines two images vertically.
-- The result image will have a height equal to the sum of the heights of both images.
-- The width will equal the largest width of the two images.
-- The area not defined in one image due to a width missmatch will be filled with the background
-- pattern.
(<->) :: Image -> Image -> Image
im_t <-> im_b 
    = vert_join im_t 
                im_b 
                ( max (image_width im_t) (image_width im_b) ) 
                ( image_height im_t + image_height im_b )

-- | Compose any number of images horizontally.
horiz_cat :: [Image] -> Image
horiz_cat = foldr (<|>) EmptyImage

-- | Compose any number of images vertically.
vert_cat :: [Image] -> Image
vert_cat = foldr (<->) EmptyImage

-- | an image of a single character. This is a standard Haskell 31-bit character assumed to be in
-- the ISO-10646 encoding.
char :: Attr -> Char -> Image
char !a !c = 
    let display_width = safe_wcwidth c
    in HorizText a (Seq.singleton (c, display_width)) display_width 1

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
iso_10646_string !a !str = 
    let display_text = Seq.fromList $ map (\c -> (c, safe_wcwidth c)) str
    in horiz_text a display_text (safe_wcswidth str)

-- | Alias for iso_10646_string. Since the usual case is that a literal string like "foo" is
-- represented internally as a list of ISO 10646 31 bit characters.  
--
-- Note: Keep in mind that GHC will compile source encoded as UTF-8 but the literal strings, while
-- UTF-8 encoded in the source, will be transcoded to a ISO 10646 31 bit characters runtime
-- representation.
string :: Attr -> String -> Image
string = iso_10646_string

-- | A string of characters layed out on a single row. The string is assumed to be a sequence of
-- UTF-8 characters.
utf8_string :: Attr -> [Word8] -> Image
utf8_string !a !str = string a ( decode str )

-- | Returns the display width of a character. Assumes all characters with unknown widths are 0 width
safe_wcwidth :: Char -> Word
safe_wcwidth c = case wcwidth c of
    i   | i < 0 -> 0 
        | otherwise -> toEnum i

-- | Returns the display width of a string. Assumes all characters with unknown widths are 0 width
safe_wcswidth :: String -> Word
safe_wcswidth str = case wcswidth str of
    i   | i < 0 -> 0 
        | otherwise -> toEnum i

-- | Renders a UTF-8 encoded bytestring. 
utf8_bytestring :: Attr -> BS.ByteString -> Image
utf8_bytestring !a !bs = string a (UTF8.toString $ UTF8.fromRep bs)

-- | creates a fill of the specified character. The dimensions are in number of characters wide and
-- number of rows high.
--
-- Unlike the Background fill character this character can have double column display width.
char_fill :: Enum d => Attr -> Char -> d -> d -> Image
char_fill !a !c w h = 
    vert_cat $ replicate (fromEnum h) $ horiz_cat $ replicate (fromEnum w) $ char a c

-- | The empty image. Useful for fold combinators. These occupy no space nor define any display
-- attributes.
empty_image :: Image 
empty_image = EmptyImage

-- | Apply the given offset to the image.
translate :: (Int, Int) -> Image -> Image
translate v i = Translation v i

-- | Ensure an image is no larger than the provided size. If the image is larger then crop.
crop :: (Word, Word) -> Image -> Image
crop (0,_) _ = EmptyImage
crop (_,0) _ = EmptyImage
crop v (ImageCrop _size i) = ImagePad (min (fst v) (fst _size), min (snd v) (snd _size)) i
crop v (ImagePad _size i) = ImagePad (min (fst v) (fst _size), min (snd v) (snd _size)) i
crop v i = ImagePad v i

-- | Ensure an image is at least the provided size. If the image is smaler then pad.
pad :: (Word, Word) -> Image -> Image
pad (0,_) _ = EmptyImage
pad (_,0) _ = EmptyImage
pad v (ImagePad _size i) = ImagePad (max (fst v) (fst _size), max (snd v) (snd _size)) i
pad v i = ImagePad v i

