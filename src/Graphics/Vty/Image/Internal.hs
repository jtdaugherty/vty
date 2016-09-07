{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_HADDOCK hide #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

module Graphics.Vty.Image.Internal where

import Graphics.Vty.Attributes
import Graphics.Text.Width

import Control.DeepSeq

import qualified Data.Text.Lazy as TL

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

-- | A display text is a Data.Text.Lazy
--
-- TODO(corey): hm. there is an explicit equation for each type which goes to a lazy text. Each
-- application probably uses a single type. Perhaps parameterize the entire vty interface by the
-- input text type?
-- TODO: Try using a builder instead of a TL.Text instance directly. That might improve performance
-- for the usual case of appending a bunch of characters with the same attribute together.
type DisplayText = TL.Text

-- TODO: store a skip list in HorizText(?)
-- TODO: represent display strings containing chars that are not 1 column chars as a separate
-- display string value?
clipText :: DisplayText -> Int -> Int -> DisplayText
clipText txt leftSkip rightClip =
    -- CPS would clarify this I think
    let (toDrop,padPrefix) = clipForCharWidth leftSkip txt 0
        txt' = if padPrefix then TL.cons '…' (TL.drop (toDrop+1) txt) else TL.drop toDrop txt
        (toTake,padSuffix) = clipForCharWidth rightClip txt' 0
        txt'' = TL.append (TL.take toTake txt') (if padSuffix then TL.singleton '…' else TL.empty)
        -- Note: some characters and zero-width and combining characters
        -- combine to the left, so keep taking characters even if the width
        -- is zero.
        clipForCharWidth w t n
            | TL.null t = (n, False)
            | w < cw    = (n, w /= 0)
            | otherwise = clipForCharWidth (w - cw) (TL.tail t) (n + 1)
            where cw = safeWcwidth (TL.head t)
    in txt''

-- | This is the internal representation of Images. Use the constructors in "Graphics.Vty.Image" to
-- create instances.
--
-- Images are:
--
-- * a horizontal span of text
--
-- * a horizontal or vertical join of two images
--
-- * a two dimensional fill of the 'Picture's background character
--
-- * a cropped image
--
-- * an empty image of no size or content.
data Image = 
    -- | A horizontal text span is always >= 1 column and has a row height of 1.
      HorizText
      { attr :: Attr
      -- | The text to display. The display width of the text is always outputWidth.
      , displayText :: DisplayText
      -- | The number of display columns for the text. Always > 0.
      , outputWidth :: Int
      -- | the number of characters in the text. Always > 0.
      , charWidth :: Int
      }
    -- | A horizontal join can be constructed between any two images. However a HorizJoin instance is
    -- required to be between two images of equal height. The horizJoin constructor adds background
    -- fills to the provided images that assure this is true for the HorizJoin value produced.
    | HorizJoin
      { partLeft :: Image 
      , partRight :: Image
      , outputWidth :: Int -- ^ imageWidth partLeft == imageWidth partRight. Always > 1
      , outputHeight :: Int -- ^ imageHeight partLeft == imageHeight partRight. Always > 0
      }
    -- | A veritical join can be constructed between any two images. However a VertJoin instance is
    -- required to be between two images of equal width. The vertJoin constructor adds background
    -- fills to the provides images that assure this is true for the VertJoin value produced.
    | VertJoin
      { partTop :: Image
      , partBottom :: Image
      , outputWidth :: Int -- ^ imageWidth partTop == imageWidth partBottom. always > 0
      , outputHeight :: Int -- ^ imageHeight partTop == imageHeight partBottom. always > 1
      }
    -- | A background fill will be filled with the background char. The background char is
    -- defined as a property of the Picture this Image is used to form.
    | BGFill
      { outputWidth :: Int -- ^ always > 0
      , outputHeight :: Int -- ^ always > 0
      }
    -- | Crop an image horizontally to a size by reducing the size from the right.
    | CropRight
      { croppedImage :: Image
      -- | Always < imageWidth croppedImage > 0
      , outputWidth :: Int
      , outputHeight :: Int -- ^ imageHeight croppedImage
      }
    -- | Crop an image horizontally to a size by reducing the size from the left.
    | CropLeft
      { croppedImage :: Image
      -- | Always < imageWidth croppedImage > 0
      , leftSkip :: Int
      -- | Always < imageWidth croppedImage > 0
      , outputWidth :: Int
      , outputHeight :: Int
      }
    -- | Crop an image vertically to a size by reducing the size from the bottom
    | CropBottom
      { croppedImage :: Image
      -- | imageWidth croppedImage
      , outputWidth :: Int
      -- | height image is cropped to. Always < imageHeight croppedImage > 0
      , outputHeight :: Int
      }
    -- | Crop an image vertically to a size by reducing the size from the top
    | CropTop
      { croppedImage :: Image
      -- | Always < imageHeight croppedImage > 0
      , topSkip :: Int
      -- | imageWidth croppedImage
      , outputWidth :: Int
      -- | Always < imageHeight croppedImage > 0
      , outputHeight :: Int
      }
    -- | The empty image
    --
    -- The combining operators identity constant. 
    -- EmptyImage <|> a = a
    -- EmptyImage <-> a = a
    -- 
    -- Any image of zero size equals the empty image.
    | EmptyImage
    deriving Eq

instance Show Image where
    show ( HorizText { attr, displayText, outputWidth, charWidth } )
        = "HorizText " ++ show displayText
                       ++ "@(" ++ show attr ++ ","
                               ++ show outputWidth ++ ","
                               ++ show charWidth ++ ")"
    show ( BGFill { outputWidth, outputHeight } )
        = "BGFill (" ++ show outputWidth ++ "," ++ show outputHeight ++ ")"
    show ( HorizJoin { partLeft = l, partRight = r, outputWidth = c } )
        = "HorizJoin " ++ show c ++ " (" ++ show l ++ " <|> " ++ show r ++ ")"
    show ( VertJoin { partTop = t, partBottom = b, outputWidth = c, outputHeight = r } )
        = "VertJoin [" ++ show c ++ ", " ++ show r ++ "] (" ++ show t ++ ") <-> (" ++ show b ++ ")"
    show ( CropRight { croppedImage, outputWidth, outputHeight } )
        = "CropRight [" ++ show outputWidth ++ "," ++ show outputHeight ++ "]"
                     ++ " (" ++ show croppedImage ++ ")"
    show ( CropLeft { croppedImage, leftSkip, outputWidth, outputHeight } )
        = "CropLeft [" ++ show leftSkip ++ "," ++ show outputWidth ++ "," ++ show outputHeight ++ "]"
                    ++ " (" ++ show croppedImage ++ ")"
    show ( CropBottom { croppedImage, outputWidth, outputHeight } )
        = "CropBottom [" ++ show outputWidth ++ "," ++ show outputHeight ++ "]"
                      ++ " (" ++ show croppedImage ++ ")"
    show ( CropTop { croppedImage, topSkip, outputWidth, outputHeight } )
        = "CropTop [" ++ show topSkip ++ "," ++ show outputWidth ++ "," ++ show outputHeight ++ "]"
                   ++ " (" ++ show croppedImage ++ ")"
    show ( EmptyImage ) = "EmptyImage"

-- | pretty print just the structure of an image.
ppImageStructure :: Image -> String
ppImageStructure inImg = go 0 inImg
    where
        go indent img = tab indent ++ pp indent img
        tab indent = concat $ replicate indent "  "
        pp _ (HorizText {outputWidth}) = "HorizText(" ++ show outputWidth ++ ")"
        pp _ (BGFill {outputWidth, outputHeight})
            = "BGFill(" ++ show outputWidth ++ "," ++ show outputHeight ++ ")"
        pp i (HorizJoin {partLeft = l, partRight = r, outputWidth = c})
            = "HorizJoin(" ++ show c ++ ")\n" ++ go (i+1) l ++ "\n" ++ go (i+1) r
        pp i (VertJoin {partTop = t, partBottom = b, outputWidth = c, outputHeight = r})
            = "VertJoin(" ++ show c ++ ", " ++ show r ++ ")\n"
              ++ go (i+1) t ++ "\n"
              ++ go (i+1) b
        pp i (CropRight {croppedImage, outputWidth, outputHeight})
            = "CropRight(" ++ show outputWidth ++ "," ++ show outputHeight ++ ")\n"
              ++ go (i+1) croppedImage
        pp i (CropLeft {croppedImage, leftSkip, outputWidth, outputHeight})
            = "CropLeft(" ++ show leftSkip ++ "->" ++ show outputWidth ++ "," ++ show outputHeight ++ ")\n"
              ++ go (i+1) croppedImage
        pp i (CropBottom {croppedImage, outputWidth, outputHeight})
            = "CropBottom(" ++ show outputWidth ++ "," ++ show outputHeight ++ ")\n"
              ++ go (i+1) croppedImage
        pp i (CropTop {croppedImage, topSkip, outputWidth, outputHeight})
            = "CropTop("++ show outputWidth ++ "," ++ show topSkip ++ "->" ++ show outputHeight ++ ")\n"
              ++ go (i+1) croppedImage
        pp _ EmptyImage = "EmptyImage"
        
instance NFData Image where
    rnf EmptyImage = ()
    rnf (CropRight i w h) = i `deepseq` w `seq` h `seq` ()
    rnf (CropLeft i s w h) = i `deepseq` s `seq` w `seq` h `seq` ()
    rnf (CropBottom i w h) = i `deepseq` w `seq` h `seq` ()
    rnf (CropTop i s w h) = i `deepseq` s `seq` w `seq` h `seq` ()
    rnf (BGFill w h) = w `seq` h `seq` ()
    rnf (VertJoin t b w h) = t `deepseq` b `deepseq` w `seq` h `seq` ()
    rnf (HorizJoin l r w h) = l `deepseq` r `deepseq` w `seq` h `seq` ()
    rnf (HorizText a s w cw) = a `seq` s `deepseq` w `seq` cw `seq` ()

-- | The width of an Image. This is the number display columns the image will occupy.
imageWidth :: Image -> Int
imageWidth HorizText { outputWidth = w } = w
imageWidth HorizJoin { outputWidth = w } = w
imageWidth VertJoin { outputWidth = w } = w
imageWidth BGFill { outputWidth = w } = w
imageWidth CropRight { outputWidth  = w } = w
imageWidth CropLeft { outputWidth  = w } = w
imageWidth CropBottom { outputWidth = w } = w
imageWidth CropTop { outputWidth = w } = w
imageWidth EmptyImage = 0

-- | The height of an Image. This is the number of display rows the image will occupy.
imageHeight :: Image -> Int
imageHeight HorizText {} = 1
imageHeight HorizJoin { outputHeight = h } = h
imageHeight VertJoin { outputHeight = h } = h
imageHeight BGFill { outputHeight = h } = h
imageHeight CropRight { outputHeight  = h } = h
imageHeight CropLeft { outputHeight  = h } = h
imageHeight CropBottom { outputHeight = h } = h
imageHeight CropTop { outputHeight = h } = h
imageHeight EmptyImage = 0

-- | Append in the Monoid instance is equivalent to <->. 
instance Monoid Image where
    mempty = EmptyImage
    mappend = vertJoin

-- | combines two images side by side
--
-- Combines text chunks where possible. Assures outputWidth and outputHeight properties are not
-- violated.
--
-- The result image will have a width equal to the sum of the two images width.  And the height will
-- equal the largest height of the two images.  The area not defined in one image due to a height
-- missmatch will be filled with the background pattern.
--
-- TODO: the bg fill is biased towards top to bottom languages(?)
horizJoin :: Image -> Image -> Image
horizJoin EmptyImage i          = i
horizJoin i          EmptyImage = i
horizJoin i0@(HorizText a0 t0 w0 cw0) i1@(HorizText a1 t1 w1 cw1)
    | a0 == a1 = HorizText a0 (TL.append t0 t1) (w0 + w1) (cw0 + cw1)
    -- TODO: assumes horiz text height is always 1
    | otherwise  = HorizJoin i0 i1 (w0 + w1) 1
horizJoin i0 i1
    -- If the images are of the same height then no padding is required
    | h0 == h1 = HorizJoin i0 i1 w h0
    -- otherwise one of the images needs to be padded to the right size.
    | h0 < h1  -- Pad i0
        = let padAmount = h1 - h0
          in HorizJoin (VertJoin i0 (BGFill w0 padAmount) w0 h1) i1 w h1
    | h0 > h1  -- Pad i1
        = let padAmount = h0 - h1
          in HorizJoin i0 (VertJoin i1 (BGFill w1 padAmount) w1 h0) w h0
    where
        w0 = imageWidth i0
        w1 = imageWidth i1
        w   = w0 + w1
        h0 = imageHeight i0
        h1 = imageHeight i1
horizJoin _ _ = error "horizJoin applied to undefined values."

-- | combines two images vertically
--
-- The result image will have a height equal to the sum of the heights of both images.
-- The width will equal the largest width of the two images.
-- The area not defined in one image due to a width missmatch will be filled with the background
-- pattern.
--
-- TODO: the bg fill is biased towards right to left languages(?)
vertJoin :: Image -> Image -> Image
vertJoin EmptyImage i          = i
vertJoin i          EmptyImage = i
vertJoin i0 i1
    -- If the images are of the same width then no background padding is required
    | w0 == w1 = VertJoin i0 i1 w0 h
    -- Otherwise one of the images needs to be padded to the size of the other image.
    | w0 < w1
        = let padAmount = w1 - w0
          in VertJoin (HorizJoin i0 (BGFill padAmount h0) w1 h0) i1 w1 h
    | w0 > w1
        = let padAmount = w0 - w1
          in VertJoin i0 (HorizJoin i1 (BGFill padAmount h1) w0 h1) w0 h
    where
        w0 = imageWidth i0
        w1 = imageWidth i1
        h0 = imageHeight i0
        h1 = imageHeight i1
        h   = h0 + h1
vertJoin _ _ = error "vertJoin applied to undefined values."

