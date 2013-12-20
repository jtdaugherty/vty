{-# LANGUAGE BangPatterns #-}
-- | The 'Picture' data structure is representative of the final terminal view.
--
-- A 'Picture' is a background paired with a layer of 'Image's.
module Graphics.Vty.Picture ( module Graphics.Vty.Picture
                            , module Graphics.Vty.Image
                            )
    where

import Graphics.Vty.Image

import Control.DeepSeq

-- | The type of images to be displayed using 'update'.  
--
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

-- | a picture with no cursor, background or image layers
empty_picture :: Picture
empty_picture = Picture NoCursor [] ClearBackground

-- | The given 'Image' is added as the top layer of the 'Picture'
add_to_top :: Picture -> Image -> Picture
add_to_top p i = p {pic_layers = i : pic_layers p}

-- | The given 'Image' is added as the bottom layer of the 'Picture'
add_to_bottom :: Picture -> Image -> Picture
add_to_bottom p i = p {pic_layers = pic_layers p ++ [i]}

-- | Create a picture for display for the given image. The picture will not have a displayed cursor
-- and no background pattern (ClearBackground) will be used.
pic_for_image :: Image -> Picture
pic_for_image i = Picture 
    { pic_cursor = NoCursor
    , pic_layers = [i]
    , pic_background = ClearBackground
    }

-- | Create a picture for display with the given layers. Ordered top to bottom.
--
-- The picture will not have a displayed cursor and no background apttern (ClearBackgroun) will be
-- used.
-- 
-- The first 'Image' is the top layer.
pic_for_layers :: [Image] -> Picture
pic_for_layers is = Picture 
    { pic_cursor = NoCursor
    , pic_layers = is
    , pic_background = ClearBackground
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

-- | A 'Picture' has a background pattern. The background is either ClearBackground. Which shows the
-- layer below or is blank if the bottom layer. Or the background pattern is a character and a
-- display attribute. If the display attribute used previously should be used for a background fill
-- then use `current_attr` for the background attribute.
--
-- \todo The current attribute is always set to the default attributes at the start of updating the
-- screen to a picture.
data Background
    = Background 
    { background_char :: Char
    , background_attr :: Attr
    }
     -- | A ClearBackground is: 
     --
     -- * the space character if there are remaining non-skip ops
     --
     -- * End of line if there are no remaining non-skip ops.
    | ClearBackground

instance NFData Background where
    rnf (Background c a) = c `seq` a `seq` ()
    rnf ClearBackground = ()

-- | Compatibility with applications that do not use more than a single layer.
pic_image :: Picture -> Image
pic_image = head . pic_layers
