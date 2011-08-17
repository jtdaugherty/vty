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

import Graphics.Vty.Image
import Graphics.Vty.Picture
import Graphics.Vty.DisplayRegion

import Codec.Binary.UTF8.String ( encode )

import Control.Monad ( forM_ )
import Control.Monad.ST.Strict

import Data.Vector (Vector)
import qualified Data.Vector as Vector hiding ( take, replicate )
import Data.Vector.Mutable ( MVector(..))
import qualified Data.Vector.Mutable as Vector

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BInt
import qualified Data.Foldable as Foldable
import qualified Data.String.UTF8 as UTF8
import Data.Word

import Foreign.Storable ( pokeByteOff )

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

data DisplayOps = DisplayOps
    { effected_region :: DisplayRegion 
    , display_ops :: RowOps
    }

-- vector of span operation vectors. One per row of the screen.
type RowOps = Vector SpanOps

type MRowOps s = MVector s SpanOps

-- vector of span operations. executed in succession
type SpanOps = Vector SpanOp

type MSpanOps s = MVector s SpanOp

instance Show DisplayOps where
    show (DisplayOps _ the_row_ops)
        = "{ " ++ (show $ Vector.map (\ops -> show ops ++ "; " ) the_row_ops) ++ " }"

instance Show SpanOp where
    show (AttributeChange attr) = show attr
    show (TextSpan ow cw _) = "TextSpan " ++ show ow ++ " " ++ show cw

span_ops_columns :: DisplayOps -> Word
span_ops_columns ops = region_width $ effected_region ops

span_ops_rows :: DisplayOps -> Word
span_ops_rows ops = region_height $ effected_region ops

span_ops_effected_columns :: SpanOps -> Word
span_ops_effected_columns in_ops = Vector.foldl' span_ops_effected_columns' 0 in_ops
    where 
        span_ops_effected_columns' t (TextSpan w _ _ ) = t + w
        span_ops_effected_columns' t _ = t

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

-- used to determine the width of a span operation , if it has one. 
span_op_has_width :: SpanOp -> Maybe (Word, Word)
span_op_has_width (TextSpan ow cw _) = Just (cw, ow)
span_op_has_width _ = Nothing

-- returns the number of columns to the character at the given position in the span op
columns_to_char_offset :: Word -> SpanOp -> Word
columns_to_char_offset cx (TextSpan _ _ utf8_str) =
    let str = UTF8.toString utf8_str
    in toEnum $! sum $! map wcwidth $! take (fromEnum cx) str
columns_to_char_offset _cx _ = error "columns_to_char_offset applied to span op without width"

-- | Produces the span ops that will render the given picture, possibly cropped or padded, into the
-- specified region.
spans_for_pic :: Picture -> DisplayRegion -> DisplayOps
spans_for_pic pic r = DisplayOps r $ Vector.create (build_spans pic r)

build_spans :: Picture -> DisplayRegion -> ST s (MRowOps s)
build_spans pic region = do
    -- m for mutable! ;-)
    mrow_ops <- Vector.replicate (fromEnum $ region_height region) Vector.empty
    -- XXX: I think building the span operations in display order would provide better performance.
    -- However, I got stuck trying to implement an algorithm that did this. This will be considered
    -- as a possible future optimization. 
    --
    -- A depth first traversal of the image is performed.  ordered according to the column range
    -- defined by the image from least to greatest.  The output row ops will at least have the
    -- region of the image specified. Iterate over all output rows and output background fills for
    -- all unspecified columns.
    --
    -- The images are made into span operations from left to right. It's possible that this could
    -- easily be made to assure top to bottom output as well. 
    if region_height region > 0
        then do 
            -- The ops builder recursively descends the image and outputs span ops that would
            -- display that image. The number of columns remaining in this row before exceeding the
            -- bounds is also provided. This is used to clip the span ops produced to the display.
            -- The skip dimensions provided do....???
            _ <- row_ops_for_image mrow_ops 
                                   (pic_image pic)
                                   (pic_background pic) 
                                   region 
                                   (0,0) 
                                   0 
                                   (region_width region)
                                   (fromEnum $ region_height region)
            -- Fill in any unspecified columns with the background pattern.
            forM_ [0 .. (fromEnum $ region_height region - 1)] $! \row -> do
                end_x <- Vector.read mrow_ops row >>= return . span_ops_effected_columns
                if end_x < region_width region 
                    then snoc_bg_fill mrow_ops (pic_background pic) (region_width region - end_x) row
                    else return ()
        else return ()
    return mrow_ops

row_ops_for_image :: MRowOps s -> Image -> Background -> DisplayRegion -> (Word, Word) -> Int -> Word -> Int -> ST s (Word, Word)
row_ops_for_image mrow_ops                      -- the image to output the ops to
                  image                         -- the image to rasterize in column order to mrow_ops
                  bg                            -- the background fill
                  region                        -- ???
                  skip_dim@(skip_row,skip_col)  -- the number of rows 
                  y                             -- ???
                  remaining_columns             -- ???
                  remain_rows
    | remaining_columns == 0 = return skip_dim
    | remain_rows == 0  = return skip_dim
    | y >= fromEnum (region_height region) = return skip_dim
    | otherwise = case image of
        EmptyImage -> return skip_dim
        -- The width provided is the number of columns this text span will occupy when displayed.
        -- if this is greater than the number of remaining columsn the output has to be produced a
        -- character at a time.
        HorizText a text_str _ _ -> do
            if skip_row > 0
                then return (skip_row - 1, skip_col)
                else do
                    skip_col' <- snoc_text_span a text_str mrow_ops skip_col y remaining_columns
                    return (skip_row, skip_col')
        VertJoin top_image bottom_image _ _ -> do
            (skip_row',skip_col') <- row_ops_for_image mrow_ops 
                                                       top_image
                                                       bg 
                                                       region 
                                                       skip_dim 
                                                       y 
                                                       remaining_columns
                                                       remain_rows
            let top_height = (fromEnum $! image_height top_image) - (fromEnum $! skip_row - skip_row')
            (skip_row'',skip_col'') <- row_ops_for_image mrow_ops 
                                                         bottom_image
                                                         bg 
                                                         region 
                                                         (skip_row', skip_col) 
                                                         (y + top_height)
                                                         remaining_columns
                                                         (remain_rows - top_height)
            return (skip_row'', min skip_col' skip_col'')
        HorizJoin l r _ _ -> do
            (skip_row',skip_col') <- row_ops_for_image mrow_ops l bg region skip_dim y remaining_columns remain_rows
            -- Don't output the right part unless there is at least a single column left after
            -- outputting the left part.
            if image_width l - (skip_col - skip_col') > remaining_columns
                then return (skip_row,skip_col')
                else do
                    (skip_row'',skip_col'') <- row_ops_for_image mrow_ops r bg region (skip_row, skip_col') y (remaining_columns - image_width l + (skip_col - skip_col')) remain_rows
                    return (min skip_row' skip_row'', skip_col'')
        BGFill width height -> do
            let min_height = if y + (fromEnum height) > (fromEnum $! region_height region)
                                then region_height region - (toEnum y)
                                else min height (toEnum remain_rows)
                min_width = min width remaining_columns
                actual_height = if skip_row > min_height
                                    then 0
                                    else min_height - skip_row
                actual_width = if skip_col > min_width
                                    then 0
                                    else min_width - skip_col
            forM_ [y .. y + fromEnum actual_height - 1] $! \y' -> snoc_bg_fill mrow_ops bg actual_width y'
            let skip_row' = if actual_height > skip_row
                                then 0
                                else skip_row - min_height
                skip_col' = if actual_width > skip_col
                                then 0
                                else skip_col - min_width
            return (skip_row',skip_col')
        Translation (dx,dy) i -> do
            if dx < 0
                -- Translation left
                -- Extract the delta and add it to skip_col.
                then row_ops_for_image mrow_ops (translate (0, dy) i) bg region (skip_row, skip_col + dw) y remaining_columns remain_rows
                -- Translation right
                else if dy < 0
                        -- Translation up
                        -- Extract the delta and add it to skip_row.
                        then row_ops_for_image mrow_ops (translate (dx, 0) i) bg region (skip_row + dh, skip_col) y remaining_columns remain_rows
                        -- Translation down
                        -- Pad the start of lines and above the image with a
                        -- background_fill image
                        else row_ops_for_image mrow_ops (background_fill ow dh <-> (background_fill dw ih <|> i)) bg region skip_dim y remaining_columns remain_rows
            where
                dw = toEnum $ abs dx
                dh = toEnum $ abs dy
                ow = image_width image
                ih = image_height i
        ImageCrop (max_w,max_h) i ->
            row_ops_for_image mrow_ops i bg region skip_dim y (min remaining_columns max_w) (min remain_rows $ fromEnum max_h)
        ImagePad (min_w,min_h) i -> do
            let hpad = if image_width i < min_w
                        then background_fill (min_w - image_width i) (image_height i)
                        else empty_image
            let vpad = if image_height i < min_h
                        then background_fill (image_width i) (min_h - image_height i)
                        else empty_image
            row_ops_for_image mrow_ops ((i <|> hpad) <-> vpad) bg region skip_dim y remaining_columns remain_rows

snoc_text_span :: Attr           -- the display attributes of the text span
                -> DisplayString -- the text to output
                -> MRowOps s     -- the display operations to add to
                -> Word          -- the number of display columns in the text span to 
                                 -- skip before outputting
                -> Int           -- the row of the display operations to add to
                -> Word          -- the number of columns from the next column to be 
                                 -- defined to the end of the display for the row.
                -> ST s Word
snoc_text_span a text_str mrow_ops columns_to_skip y remaining_columns = do
    {-# SCC "snoc_text_span-pre" #-} snoc_op mrow_ops y $! AttributeChange a
    -- At most a text span will consist of remaining_columns characters
    -- we keep track of the position of the next character.
    let max_len :: Int = fromEnum remaining_columns
    mspan_chars <- Vector.new max_len
    ( used_display_columns, display_columns_skipped, used_char_count ) 
        <- {-# SCC "snoc_text_span-foldlM" #-} Foldable.foldlM (build_text_span mspan_chars) ( 0, 0, 0 ) text_str
    -- once all characters have been output to mspan_chars we grab the used head 
    out_text <- Vector.unsafeFreeze $! Vector.take used_char_count mspan_chars
    -- convert to UTF8 bytestring.
    -- This could be made faster. Hopefully the optimizer does a fair job at fusing the fold
    -- contained in fromString with the unfold in toList. No biggy right now then.
    {-# SCC "snoc_text_span-post" #-} snoc_op mrow_ops y $! TextSpan used_display_columns (toEnum used_char_count)
                       $! UTF8.fromString 
                       $! Vector.toList out_text
    return $ columns_to_skip - display_columns_skipped
    where
        build_text_span mspan_chars (!used_display_columns, !display_columns_skipped, !used_char_count) 
                                    (out_char, char_display_width) = {-# SCC "build_text_span" #-}
            -- Only valid if the maximum width of a character is 2 display columns.
            -- XXX: Optimize into a skip pass then clipped fill pass
            if display_columns_skipped == columns_to_skip
                then if used_display_columns == remaining_columns
                        then return $! ( used_display_columns, display_columns_skipped, used_char_count )
                        else if ( used_display_columns + char_display_width ) > remaining_columns
                                then do
                                    Vector.unsafeWrite mspan_chars used_char_count '…'
                                    return $! ( used_display_columns
                                              , display_columns_skipped
                                              , used_char_count  + 1
                                              )
                                else do
                                    Vector.unsafeWrite mspan_chars used_char_count out_char
                                    return $! ( used_display_columns + char_display_width
                                              , display_columns_skipped
                                              , used_char_count + 1
                                              )
                else if (display_columns_skipped + char_display_width) > columns_to_skip
                        then do
                            Vector.unsafeWrite mspan_chars used_char_count '…'
                            return $! ( used_display_columns + 1
                                      , columns_to_skip
                                      , used_char_count + 1
                                      )
                        else return $ ( used_display_columns
                                      , display_columns_skipped + char_display_width
                                      , used_char_count
                                      )

snoc_bg_fill :: MRowOps s -> Background -> Word -> Int -> ST s ()
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

snoc_op :: MRowOps s -> Int -> SpanOp -> ST s ()
snoc_op !mrow_ops !row !op = do
    ops <- Vector.read mrow_ops row
    let ops' = Vector.snoc ops op
    Vector.write mrow_ops row ops'

