-- Copyright Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | A picture is translated into a sequences of state changes and character spans.
 - State changes are currently limited to new attribute values. The attribute is applied to all
 - following spans. Including spans of the next row.  The nth element of the sequence represents the
 - nth row (from top to bottom) of the picture to render.
 -
 - A span op sequence will be defined for all rows and columns (and no more) of the region provided
 - with the picture to spans_for_pic.
 -
 - todo: Partition attribute changes into multiple categories according to the serialized
 - representation of the various attributes.
 -}
module Graphics.Vty.Span
    where

import Graphics.Vty.DisplayRegion
import Graphics.Vty.Image

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | This represents an operation on the terminal. Either an attribute change or the output of a
-- text string.
data SpanOp =
      AttributeChange !Attr
    -- | a span of UTF-8 text occupies a specific number of screen space columns. A single UTF
    -- character does not necessarially represent 1 colunm. See Codec.Binary.UTF8.Width
    -- TextSpan [output width in columns] [number of characters] [data]
    | TextSpan !Int !Int BS.ByteString
    -- | Skips the given number of columns
    -- A skip is transparent.... maybe? I am not sure how attribute changes interact.
    -- todo: separate from this type.
    | Skip !Int
    -- | Marks the end of a row. specifies how many columns are remaining. These columns will not be
    -- explicitly overwritten with the span ops. The terminal is require to assure the remaining
    -- columns are clear.
    -- todo: separate from this type.
    | RowEnd !Int
    deriving Eq

-- | vector of span operations. executed in succession. This represents the operations required to
-- render a row of the terminal. The operations in one row may effect subsequent rows.
-- EG: Setting the foreground color in one row will effect all subsequent rows until the foreground
-- color is changed.
type SpanOps = Vector SpanOp

-- | vector of span operation vectors for display. One per row of the output region.
type DisplayOps = Vector SpanOps

instance Show SpanOp where
    show (AttributeChange attr) = show attr
    show (TextSpan ow cw _) = "TextSpan(" ++ show ow ++ ", " ++ show cw ++ ")"
    show (Skip ow) = "Skip(" ++ show ow ++ ")"
    show (RowEnd ow) = "RowEnd(" ++ show ow ++ ")"

-- | Number of columns the DisplayOps are defined for
--
-- All spans are verified to define same number of columns. See: VerifySpanOps
display_ops_columns :: DisplayOps -> Int
display_ops_columns ops 
    | Vector.length ops == 0 = 0
    | otherwise              = Vector.length $ Vector.head ops

-- | Number of rows the DisplayOps are defined for
display_ops_rows :: DisplayOps -> Int
display_ops_rows ops = Vector.length ops

effected_region :: DisplayOps -> DisplayRegion
effected_region ops = DisplayRegion (display_ops_columns ops) (display_ops_rows ops)

-- | The number of columns a SpanOps effects.
span_ops_effected_columns :: SpanOps -> Int
span_ops_effected_columns in_ops = Vector.foldl' span_ops_effected_columns' 0 in_ops
    where 
        span_ops_effected_columns' t (TextSpan w _ _ ) = t + w
        span_ops_effected_columns' t (Skip w) = t + w
        span_ops_effected_columns' t (RowEnd w) = t + w
        span_ops_effected_columns' t _ = t

-- | The width of a single SpanOp in columns
span_op_has_width :: SpanOp -> Maybe (Int, Int)
span_op_has_width (TextSpan ow cw _) = Just (cw, ow)
span_op_has_width (Skip ow) = Just (ow,ow)
span_op_has_width (RowEnd ow) = Just (ow,ow)
span_op_has_width _ = Nothing

-- | returns the number of columns to the character at the given position in the span op
columns_to_char_offset :: Int -> SpanOp -> Int
columns_to_char_offset cx (TextSpan _ _ utf8_str) =
    let str = T.unpack (T.decodeUtf8 utf8_str)
    in wcswidth (take cx str)
columns_to_char_offset cx (Skip _) = cx
columns_to_char_offset cx (RowEnd _) = cx
columns_to_char_offset _cx _ = error "columns_to_char_offset applied to span op without width"

