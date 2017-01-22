{-# LANGUAGE BangPatterns #-}
-- | The 'Picture' data structure is representative of the final
-- terminal view.
--
-- A 'Picture' is a background paired with a layer of 'Image's.
module Graphics.Vty.Picture
  ( Picture(..)
  , Cursor(..)
  , Background(..)
  , emptyPicture
  , addToTop
  , addToBottom
  , picForImage
  , picForLayers
  , picImage
  )
where

import Graphics.Vty.Image
import Graphics.Vty.Attributes

import Control.DeepSeq

-- | The type of images to be displayed using 'update'.
--
-- Can be constructed directly or using `picForImage`. Which provides
-- an initial instance with reasonable defaults for picCursor and
-- picBackground.
data Picture = Picture
    { picCursor :: Cursor
    , picLayers :: [Image]
    , picBackground :: Background
    }

instance Show Picture where
    show (Picture _ layers _ ) = "Picture ?? " ++ show layers ++ " ??"

instance NFData Picture where
    rnf (Picture c l b) = c `deepseq` l `deepseq` b `deepseq` ()

-- | a picture with no cursor, background or image layers
emptyPicture :: Picture
emptyPicture = Picture NoCursor [] ClearBackground

-- | The given 'Image' is added as the top layer of the 'Picture'
addToTop :: Picture -> Image -> Picture
addToTop p i = p {picLayers = i : picLayers p}

-- | The given 'Image' is added as the bottom layer of the 'Picture'
addToBottom :: Picture -> Image -> Picture
addToBottom p i = p {picLayers = picLayers p ++ [i]}

-- | Create a picture for display for the given image. The picture
-- will not have a displayed cursor and no background pattern
-- (ClearBackground) will be used.
picForImage :: Image -> Picture
picForImage i = Picture
    { picCursor = NoCursor
    , picLayers = [i]
    , picBackground = ClearBackground
    }

-- | Create a picture for display with the given layers. Ordered top to
-- bottom.
--
-- The picture will not have a displayed cursor and no background
-- apttern (ClearBackgroun) will be used.
--
-- The first 'Image' is the top layer.
picForLayers :: [Image] -> Picture
picForLayers is = Picture
    { picCursor = NoCursor
    , picLayers = is
    , picBackground = ClearBackground
    }

-- | A picture can be configured either to not show the cursor or show
-- the cursor at the specified character position.
--
-- There is not a 1 to 1 map from character positions to a row and
-- column on the screen due to characters that take more than 1 column.
--
-- todo: The Cursor can be given a (character,row) offset outside of the
-- visible bounds of the output region. In this case the cursor will not
-- be shown.
data Cursor =
      -- | Hide the cursor
      NoCursor
      -- | Show the cursor at the given logical column accounting for
      -- char width and row
    | Cursor !Int !Int
      -- | Show the cursor at the given absolute terminal column and row
    | AbsoluteCursor !Int !Int

instance NFData Cursor where
    rnf c = c `seq` ()

-- | A 'Picture' has a background pattern. The background is either
-- ClearBackground. Which shows the layer below or is blank if the
-- bottom layer. Or the background pattern is a character and a display
-- attribute. If the display attribute used previously should be used
-- for a background fill then use `currentAttr` for the background
-- attribute.
--
-- \todo The current attribute is always set to the default attributes
-- at the start of updating the screen to a picture.
data Background
    = Background
    { backgroundChar :: Char
    , backgroundAttr :: Attr
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

-- | Compatibility with applications that do not use more than a single
-- layer.
picImage :: Picture -> Image
picImage = head . picLayers
