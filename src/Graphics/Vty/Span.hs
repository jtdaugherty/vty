-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The ops to define the content for an output region. 
module Graphics.Vty.Span
    where

import Codec.Binary.UTF8.Width ( wcwidth )

import Graphics.Vty.Image
import Graphics.Vty.Picture
import Graphics.Vty.DisplayRegion

import Codec.Binary.UTF8.String ( encode )

import Control.Monad ( forM_ )
import Control.Monad.ST.Strict

import Data.Array.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BInt
import qualified Data.Foldable as Foldable
import Data.Word
import qualified Data.ByteString.UTF8 as BSUTF8 
import qualified Data.String.UTF8 as UTF8

import Foreign.Storable ( pokeByteOff )

import GHC.Arr

{- | A picture is translated into a sequences of state changes and character spans.
 - State changes are currently limited to new attribute values. The attribute is applied to all
 - following spans. Including spans of the next row.  The nth element of the sequence represents the
 - nth row (from top to bottom) of the picture to render.
 -
 - A span op sequence will be defined for all rows and columns (and no more) of the region provided
 - with the picture to spans_for_pic.
 -
 - 
 - todo: Partition attribute changes into multiple categories according to the serialized
 - representation of the various attributes.
 -}
data SpanOpSequence = SpanOpSequence 
    { effected_region :: DisplayRegion
    , row_ops :: RowOps 
    } 

type RowOps = Array Word SpanOps
type SpanOps = [SpanOp]

span_ops_columns :: SpanOpSequence -> Word
span_ops_columns ops = region_width $ effected_region ops

span_ops_rows :: SpanOpSequence -> Word
span_ops_rows ops = region_height $ effected_region ops

span_ops_effected_columns :: [SpanOp] -> Word
span_ops_effected_columns in_ops = span_ops_effected_columns' 0 in_ops
    where 
        span_ops_effected_columns' t [] = t
        span_ops_effected_columns' t (TextSpan w _ _ : r) = span_ops_effected_columns' (t + w) r
        span_ops_effected_columns' t (_ : r) = span_ops_effected_columns' t r

-- |
-- 
-- todo: This type may need to be restructured to increase sharing in the bytestring
-- 
-- todo: Make foldable
data SpanOp =
      AttributeChange !Attr
    -- | a span of UTF-8 text occupies a specific number of screen space columns. A single UTF
    -- character does not necessarially represent 1 colunm. See Codec.Binary.UTF8.Width
    -- TextSpan [output width in columns] [number of characters] [data]
    | TextSpan !Word !Word (UTF8.UTF8 B.ByteString)
    deriving Eq

span_op_has_width :: SpanOp -> Maybe (Word, Word)
span_op_has_width (TextSpan ow cw _) = Just (cw, ow)
span_op_has_width _ = Nothing

columns_to_char_offset :: Word -> SpanOp -> Word
columns_to_char_offset cx (TextSpan _ _ utf8_str) =
    let str = UTF8.toString utf8_str
    in toEnum $ sum $ map wcwidth $ take (fromEnum cx) str
columns_to_char_offset _cx _ = error "columns_to_char_offset applied to span op without width"

-- | Produces the span ops that will render the given picture, possibly cropped or padded, into the
-- specified region.
spans_for_pic :: Picture -> DisplayRegion -> SpanOpSequence
spans_for_pic pic r = SpanOpSequence r $ runSTArray (build_spans pic r)

build_spans :: Picture -> DisplayRegion -> ST s (STArray s Word [SpanOp])
build_spans pic region = do
    -- m for mutable
    mrow_ops <- newSTArray (0, region_height region - 1) []
    -- I think this can be better optimized if the build produced span operations in display order.
    -- However, I got stuck trying to implement an algorithm that did this. This will be considered
    -- as a possible future optimization. 
    --
    -- A depth first traversal of the image is performed.
    -- ordered according to the column range defined by the image from least to greatest.
    -- The output row ops will at least have the region of the image specified. Iterate over all
    -- output rows and output background fills for all unspecified columns.
    if region_height region > 0
        then do 
            -- The ops builder recursively descends the image and outputs span ops that would
            -- display that image. The number of columns remaining in this row before exceeding the
            -- bounds is also provided. This is used to clip the span ops produced.
            ops_for_row mrow_ops (pic_background pic) region (pic_image pic) 0 (region_width region)
            -- Fill in any unspecified columns with the background pattern.
            forM_ [0 .. region_height region - 1] $ \row -> do
                end_x <- readSTArray mrow_ops row >>= return . span_ops_effected_columns
                if end_x < region_width region 
                    then snoc_bg_fill mrow_ops (pic_background pic) (region_width region - end_x) row
                    else return ()
        else return ()
    return mrow_ops

type MRowOps s = STArray s Word SpanOps

ops_for_row :: MRowOps s -> Background -> DisplayRegion -> Image -> Word -> Word -> ST s ()
ops_for_row mrow_ops bg region image y remaining_columns
    | remaining_columns == 0 = return ()
    | y >= region_height region = return ()
    | otherwise = case image of
        EmptyImage -> return ()
        -- The width provided is the number of columns this text span will occupy when displayed.
        -- if this is greater than the number of remaining columsn the output has to be produced a
        -- character at a time.
        HorizText a text_str ow cw -> do
            snoc_text_span a text_str ow cw mrow_ops y remaining_columns
        VertJoin t b _ _ -> do
            ops_for_row mrow_ops bg region t y remaining_columns
            ops_for_row mrow_ops bg region b (y + image_height t) remaining_columns
        HorizJoin l r _ _ -> do
            ops_for_row mrow_ops bg region l y remaining_columns
            -- Don't output the right part unless there is at least a single column left after
            -- outputting the left part.
            if image_width l < remaining_columns
                then ops_for_row mrow_ops bg region r y (remaining_columns - image_width l)
                else return ()
        BGFill width height -> do
            let actual_height = if y + height > region_height region 
                                    then region_height region - y
                                    else height
                actual_width = if width > remaining_columns
                                    then remaining_columns
                                    else width
            forM_ [y .. y + actual_height - 1] $ \y' -> snoc_bg_fill mrow_ops bg actual_width y'
        Translation _offset i -> ops_for_row mrow_ops bg region i y remaining_columns
        ImageCrop _size i ->
            if y >= (toEnum $ snd _size)
                then return ()
                else ops_for_row mrow_ops bg region i y (min remaining_columns $ toEnum $ fst _size)
        ImagePad (x,y) i -> do
            let hpad = if image_width i < x
                        then background_fill (x - image_width i) (image_height i)
                        else empty_image
            let vpad = if image_height i < y
                        then background_fill (image_width i) (y - image_height i)
                        else empty_image
            ops_for_row mrow_ops bg region (vert_join (horiz_join i hpad) vpad) y remaining_columns

snoc_text_span :: (Foldable.Foldable t) 
                => Attr 
                -> t Char 
                -> Word -- width of text span in output display columns
                -> Word -- width of text span in characters
                -> MRowOps s 
                -> Word 
                -> Word 
                -> ST s ()
snoc_text_span a text_str ow cw mrow_ops y remaining_columns = do
    if ow > remaining_columns
        then do
            snoc_op mrow_ops y $ AttributeChange a
            let (ow', cw', txt) = Foldable.foldl' 
                                      build_cropped_txt
                                      ( 0, 0, B.empty )
                                      text_str
            snoc_op mrow_ops y $ TextSpan ow' cw' (UTF8.fromRep txt)
        else do
            snoc_op mrow_ops y $ AttributeChange a
            let utf8_bs = ({-# SCC "BSUTF8.fromString" #-} BSUTF8.fromString) $ Foldable.foldMap (\c -> [c]) text_str
            snoc_op mrow_ops y $ TextSpan ow cw (UTF8.fromRep utf8_bs)
    where
        build_cropped_txt (ow', char_count', b0) c = {-# SCC "build_cropped_txt" #-}
            let w = wcwidth c
            -- Characters with unknown widths occupy 1 column.  
            -- 
            -- todo: Not sure if this is actually correct.
            -- I presume there is a replacement character that is output by the terminal instead of
            -- the character. If so then this replacement process may need to be implemented
            -- manually for consistent behavior across terminals.
                w' = toEnum $ if w < 0 then 1 else w
            in if (w' + ow') > remaining_columns
                then ( ow', char_count', b0 )
                else ( ow' + w', char_count' + 1, B.append b0 $ B.pack $ encode [c] )

snoc_bg_fill :: MRowOps s -> Background -> Word -> Word -> ST s ()
snoc_bg_fill _row_ops _bg 0 _row 
    = return ()
snoc_bg_fill mrow_ops (Background c back_attr) fill_length row 
    = do
        snoc_op mrow_ops row $ AttributeChange back_attr
        -- By all likelyhood the background character will be an ASCII character. Which is a single
        -- byte in utf8. Optimize for this special case.
        utf8_bs <- if c <= (toEnum 255 :: Char)
            then
                let !(c_byte :: Word8) = BInt.c2w c
                in unsafeIOToST $ do
                    BInt.create ( fromEnum fill_length ) 
                                $ \ptr -> mapM_ (\i -> pokeByteOff ptr i c_byte)
                                                [0 .. fromEnum (fill_length - 1)]
            else 
                let !(c_bytes :: [Word8]) = encode [c]
                in unsafeIOToST $ do
                    BInt.create (fromEnum fill_length * length c_bytes) 
                                $ \ptr -> mapM_ (\(i,b) -> pokeByteOff ptr i b)
                                                $ zip [0 .. fromEnum (fill_length - 1)] (cycle c_bytes)
        snoc_op mrow_ops row $ TextSpan fill_length fill_length (UTF8.fromRep utf8_bs)

snoc_op :: MRowOps s -> Word -> SpanOp -> ST s ()
snoc_op !mrow_ops !row !op = do
    ops <- readSTArray mrow_ops row
    let ops' = ops ++ [op]
    writeSTArray mrow_ops row ops'
    return ()

