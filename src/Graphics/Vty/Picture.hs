{-# LANGUAGE BangPatterns #-}
-- A 'Picture' is a background paired with a set of 'Image' layers. The
-- 'Picture' data structure is representative of the final terminal
-- view.
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

-- | A Vty picture.
--
-- These can be constructed directly or using `picForImage`.
data Picture = Picture
    { picCursor :: Cursor
    -- ^ The picture's cursor.
    , picLayers :: [Image]
    -- ^ The picture's image layers (top-most first).
    , picBackground :: Background
    -- ^ The picture's background to be displayed in locations with no
    -- Image data.
    }

instance Show Picture where
    show (Picture _ layers _ ) = "Picture ?? " ++ show layers ++ " ??"

instance NFData Picture where
    rnf (Picture c l b) = c `deepseq` l `deepseq` b `deepseq` ()

-- | A picture with no cursor, background or image layers.
emptyPicture :: Picture
emptyPicture = Picture NoCursor [] ClearBackground

-- | Add an 'Image' as the top-most layer of a 'Picture'.
addToTop :: Picture -> Image -> Picture
addToTop p i = p {picLayers = i : picLayers p}

-- | Add an 'Image' as the bottom-most layer of a 'Picture'.
addToBottom :: Picture -> Image -> Picture
addToBottom p i = p {picLayers = picLayers p ++ [i]}

-- | Create a picture from the given image. The picture will not have a
-- displayed cursor and no background pattern (ClearBackground) will be
-- used.
picForImage :: Image -> Picture
picForImage i = Picture
    { picCursor = NoCursor
    , picLayers = [i]
    , picBackground = ClearBackground
    }

-- | Create a picture with the given layers, top-most first.
--
-- The picture will not have a displayed cursor and no background
-- pattern (ClearBackgroun) will be used.
picForLayers :: [Image] -> Picture
picForLayers is = Picture
    { picCursor = NoCursor
    , picLayers = is
    , picBackground = ClearBackground
    }

-- | A picture can be configured to hide the cursor or to show the
-- cursor at the specified character position.
--
-- There is not a 1:1 map from character positions to a row and column
-- on the screen due to characters that take more than 1 column.
data Cursor =
    -- | Hide the cursor
    NoCursor
    -- | Show the cursor at the given logical column accounting for
    -- character width in the presence of multi-column characters.
    | Cursor !Int !Int
    -- | Show the cursor at the given absolute terminal column and row
    | AbsoluteCursor !Int !Int

instance NFData Cursor where
    rnf c = c `seq` ()

-- | A 'Picture' has a background pattern. The background is either:
--
-- * ClearBackground, which shows the layer below or is blank if the
--   bottom layer
-- * A character and a display attribute
--
-- If the display attribute used previously should be used for a
-- background fill then use `currentAttr` for the background attribute.
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

-- | Return the top-most 'Image' layer for a picture. This is unsafe for
-- 'Picture's without at least one layer.
--
-- This is provided for compatibility with applications that do not use
-- more than a single layer.
picImage :: Picture -> Image
picImage = head . picLayers
