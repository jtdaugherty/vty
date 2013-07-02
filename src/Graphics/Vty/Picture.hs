{-# LANGUAGE BangPatterns #-}
-- | The Picture data structure is representative of the final terminal view.
--
-- This module re-exports most of the Graphics.Vty.Image and Graphics.Vty.Attributes modules.
--
-- Copyright 2009-2010 Corey O'Connor
module Graphics.Vty.Picture ( module Graphics.Vty.Picture
                            , Image
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
                            , crop
                            , pad
                            -- | The possible display attributes used in constructing an `Image`.
                            , module Graphics.Vty.Attributes
                            )
    where

import Graphics.Vty.Attributes
import Graphics.Vty.Image

import Control.DeepSeq

-- | The type of images to be displayed using 'update'.  
-- Can be constructed directly or using `pic_for_image`. Which provides an initial instance with
-- reasonable defaults for pic_cursor and pic_background.
data Picture = Picture
    { pic_cursor :: Cursor
    , pic_layers :: [Image]
    , pic_background :: Background
    }

instance Show Picture where
    show (Picture _ layers _ ) = "Picture ?? " ++ show layers ++ " ??"

instance NFData Picture where
    rnf (Picture c l b) = c `deepseq` l `deepseq` b `deepseq` ()

-- | Create a picture for display for the given image. The picture will not have a displayed cursor
-- and the background display attribute will be `current_attr`.
pic_for_image :: Image -> Picture
pic_for_image i = Picture 
    { pic_cursor = NoCursor
    , pic_layers = [i]
    , pic_background = Background ' ' current_attr
    }

-- | A picture can be configured either to not show the cursor or show the cursor at the specified
-- character position. 
--
-- There is not a 1 to 1 map from character positions to a row and column on the screen due to
-- characters that take more than 1 column.
--
-- todo: The Cursor can be given a (character,row) offset outside of the visible bounds of the
-- output region. In this case the cursor will not be shown.
data Cursor = 
      NoCursor
    | Cursor Int Int

instance NFData Cursor where
    rnf NoCursor = ()
    rnf (Cursor w h) = w `seq` h `seq` ()

-- | Unspecified regions are filled with the picture's background pattern.  The background pattern
-- can specify a character and a display attribute. If the display attribute used previously should
-- be used for a background fill then use `current_attr` for the background attribute. This is the
-- default background display attribute.
--
-- \todo The current attribute is always set to the default attributes at the start of updating the
-- screen to a picture.
--
-- \todo The background character *must* occupy a single column and no more.
--
-- \todo background char should be optional
data Background = Background 
    { background_char :: Char
    , background_attr :: Attr
    }

instance NFData Background where
    rnf (Background c a) = c `seq` a `seq` ()

-- | Compatibility with applications that do not use more than a single layer.
pic_image :: Picture -> Image
pic_image = head . pic_layers
