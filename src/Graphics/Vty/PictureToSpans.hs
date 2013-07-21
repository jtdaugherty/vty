-- Copyright Corey O'Connor<coreyoconnor@gmail.com>
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Graphics.Vty.PictureToSpans where

import Graphics.Vty.DisplayRegion
import Graphics.Vty.Image
import Graphics.Vty.Image.Internal
import Graphics.Vty.Picture
import Graphics.Vty.Span

import Control.Applicative
import Control.Lens hiding ( op )
import Control.Monad.Reader
import Control.Monad.State.Strict hiding ( state )
import Control.Monad.ST.Strict hiding ( unsafeIOToST )

import qualified Data.Vector as Vector hiding ( take, replicate )
import Data.Vector.Mutable ( MVector(..))
import qualified Data.Vector.Mutable as Vector

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

type MRowOps s = MVector s SpanOps

type MSpanOps s = MVector s SpanOp

-- transform plus clip. More or less.
data BlitState = BlitState
    -- we always snoc to the operation vectors. Thus the column_offset = length of row at row_offset
    { _column_offset :: Int
    , _row_offset :: Int
    -- clip coordinate space is in image space. Which means it's >= 0 and < image_width.
    , _skip_columns :: Int
    -- >= 0 and < image_height
    , _skip_rows :: Int
    -- includes consideration of skip_columns. In display space.
    -- The number of columns from the next column to be defined to the end of the display for the
    -- row.
    , _remaining_columns :: Int
    -- includes consideration of skip_rows. In display space.
    , _remaining_rows :: Int
    }

makeLenses ''BlitState

data BlitEnv s = BlitEnv
    { _bg :: Background
    , _region :: DisplayRegion
    , _mrow_ops :: MRowOps s
    }

makeLenses ''BlitEnv

type BlitM s a = ReaderT (BlitEnv s) (StateT BlitState (ST s)) a

-- | Produces the span ops that will render the given picture, possibly cropped or padded, into the
-- specified region.
spans_for_pic :: Picture -> DisplayRegion -> DisplayOps
spans_for_pic pic r = DisplayOps r $ Vector.create (combined_spans_for_layers pic r)

-- | Produces the span ops for each layer then combines them.
--
-- TODO: a fold over a builder function. start with span ops that are a bg fill of the entire
-- region.
combined_spans_for_layers :: Picture -> DisplayRegion -> ST s (MRowOps s)
combined_spans_for_layers pic r = do
    layer_ops <- mapM (\layer -> build_spans layer r (pic_background pic)) (pic_layers pic)
    case layer_ops of
        []    -> fail "empty picture"
        [ops] -> return ops
        _     -> fail "TODO: picture with more than one layer not supported"

-- | Builds a vector of row operations that will output the given picture to the terminal.
--
-- Crops to the given display region.
--
-- TODO: I'm pretty sure there is an algorithm that does not require a mutable buffer.
build_spans :: Image -> DisplayRegion -> Background -> ST s (MRowOps s)
build_spans image out_region background = do
    -- First we create a mutable vector for each rows output operations.
    out_ops <- Vector.replicate (region_height out_region) Vector.empty
    -- \todo I think building the span operations in display order would provide better performance.
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
    when (region_height out_region > 0 && region_width out_region > 0) $ do
        -- The ops builder recursively descends the image and outputs span ops that would
        -- display that image. The number of columns remaining in this row before exceeding the
        -- bounds is also provided. This is used to clip the span ops produced to the display.
        let full_build = do
                start_image_build image
                -- Fill in any unspecified columns with the background pattern.
                forM_ [0 .. (region_height out_region - 1)] (add_row_completion out_region)
            init_env   = BlitEnv background out_region out_ops
            init_state = BlitState 0 0 0 0 (region_width out_region) (region_height out_region)
        _ <- runStateT (runReaderT full_build init_env) init_state
        return ()
    return out_ops

-- | Add the operations required to build a given image to the current set of row operations
-- returns the number of columns and rows contributed to the output.
start_image_build :: Image -> BlitM s ()
start_image_build image = do
    out_of_bounds <- is_out_of_bounds <$> get
    when (not out_of_bounds) $ add_maybe_clipped image

is_out_of_bounds :: BlitState -> Bool
is_out_of_bounds s
    | s ^. remaining_columns <= 0 = True
    | s ^. remaining_rows    <= 0 = True
    | otherwise                   = False

-- TODO: prove skip_columns, skip_rows == 0
-- TODO: prove remaining_columns >= image_width
-- TODO: prove remaining_rows >= image_height
add_unclipped :: Image -> BlitM s ()
add_unclipped EmptyImage = return ()
add_unclipped (HorizText a text_str _ow _cw) = do
    use row_offset >>= snoc_op (AttributeChange a)
    add_unclipped_text text_str
add_unclipped (HorizJoin part_left part_right _ow _oh) = do
    -- TODO: push into env and use
    y <- use row_offset
    add_unclipped part_left
    row_offset .= y
    add_unclipped part_right
add_unclipped (VertJoin part_top part_bottom _ow _oh) = do
    y <- use row_offset
    add_unclipped part_top
    row_offset .= y + image_height part_top
    add_unclipped part_bottom
    -- TODO: assumes background character is 1 column
add_unclipped (BGFill ow oh) = do
    Background c a <- view bg
    y <- use row_offset
    let op = TextSpan ow ow (T.encodeUtf8 $ T.replicate (fromIntegral ow) (T.singleton c))
    forM_ [y..y+oh-1] $ \row -> do
        snoc_op (AttributeChange a) row
        snoc_op op row
-- TODO: we know it's clipped actually, the equations that are exposed that introduce a
-- Crop all assure the image exceeds the crop in the relevant direction.
add_unclipped CropRight {cropped_image, output_width} = do
    remaining_columns .= output_width
    add_maybe_clipped cropped_image
add_unclipped CropLeft {cropped_image, left_skip} = do
    skip_columns .= left_skip
    add_maybe_clipped cropped_image
add_unclipped CropBottom {cropped_image, output_height} = do
    remaining_rows .= output_height
    add_maybe_clipped cropped_image
add_unclipped CropTop {cropped_image, top_skip} = do
    skip_rows .= top_skip
    add_maybe_clipped cropped_image

-- TODO: prove this cannot be called in an out of bounds case.
add_maybe_clipped :: forall s . Image -> BlitM s ()
add_maybe_clipped EmptyImage = return ()
add_maybe_clipped (HorizText a text_str ow _cw) = do
    -- TODO: assumption that text spans are only 1 row high.
    s <- use skip_rows
    when (s < 1) $ do
        use row_offset >>= snoc_op (AttributeChange a)
        left_clip <- use skip_columns
        right_clip <- use remaining_columns
        let left_clipped = left_clip > 0
            right_clipped = (ow - left_clip) > right_clip
        if left_clipped || right_clipped
            then let text_str' = clip_text text_str left_clip right_clip
                 in add_unclipped_text text_str'
            else add_unclipped_text text_str
add_maybe_clipped (VertJoin top_image bottom_image _ow oh) = do
    add_maybe_clipped_join "vert_join" skip_rows remaining_rows row_offset
                           (image_height top_image)
                           top_image
                           bottom_image
                           oh
add_maybe_clipped (HorizJoin left_image right_image ow _oh) = do
    add_maybe_clipped_join "horiz_join" skip_columns remaining_columns column_offset
                           (image_width left_image)
                           left_image
                           right_image
                           ow
add_maybe_clipped BGFill {output_width, output_height} = do
    s <- get
    let output_width'  = min (output_width  - s^.skip_columns) (s^.remaining_columns)
        output_height' = min (output_height - s^.skip_rows   ) (s^.remaining_rows)
    add_unclipped (BGFill output_width' output_height')
add_maybe_clipped CropRight {cropped_image, output_width} = do
    remaining_columns .= output_width
    add_maybe_clipped cropped_image
add_maybe_clipped CropLeft {cropped_image, left_skip} = do
    skip_columns += left_skip
    add_maybe_clipped cropped_image
add_maybe_clipped CropBottom {cropped_image, output_height} = do
    remaining_rows .= output_height
    add_maybe_clipped cropped_image
add_maybe_clipped CropTop {cropped_image, top_skip} = do
    skip_rows += top_skip
    add_maybe_clipped cropped_image

add_maybe_clipped_join :: forall s . String 
                       -> Lens BlitState BlitState Int Int
                       -> Lens BlitState BlitState Int Int
                       -> Lens BlitState BlitState Int Int
                       -> Int
                       -> Image
                       -> Image
                       -> Int
                       -> BlitM s ()
add_maybe_clipped_join name skip remaining offset i0_dim i0 i1 size = do
    state <- get
    when (state^.remaining == 0) $ fail $ name ++ " with remaining == 0"
    case state^.skip of
        s | s >= size -> fail $ name ++ " on fully clipped"
          -- TODO: check if clipped in other dim. if not use add_unclipped
          | s == 0    -> case state^.remaining of
                r | r > i0_dim -> do
                        add_maybe_clipped i0
                        put $ state & offset +~ i0_dim & remaining -~ i0_dim
                        add_maybe_clipped i1
                  | otherwise  -> add_maybe_clipped i0
          | s >= i0_dim -> do
                put $ state & skip -~ i0_dim
                add_maybe_clipped i1
          | otherwise  -> case i0_dim - s of
                i0_dim' | state^.remaining <= i0_dim' -> add_maybe_clipped i0
                        | otherwise                   -> do
                            add_maybe_clipped i0
                            put $ state & offset +~ i0_dim' & remaining -~ i0_dim'
                            add_maybe_clipped i1

-- TODO: store a skip list in HorizText(?)
-- TODO: represent display strings containing chars that are not 1 column chars as a separate
-- display string value?
-- TODO: assumes max column width is 2
clip_text :: DisplayText -> Int -> Int -> DisplayText
clip_text txt left_skip right_clip =
    -- CPS would clarify this I think
    let (to_drop,pad_prefix) = clip_for_char_width left_skip txt 0
        txt' = if pad_prefix then TL.cons '…' (TL.drop (to_drop+1) txt) else TL.drop to_drop txt
        (to_take,pad_suffix) = clip_for_char_width right_clip txt' 0
        txt'' = TL.append (TL.take to_take txt') (if pad_suffix then TL.singleton '…' else TL.empty)
        clip_for_char_width 0 _ n = (n, False)
        clip_for_char_width w t n
            | w <  cw = (n, True)
            | w == cw = (n+1, False)
            | w >  cw = clip_for_char_width (w - cw) (TL.tail t) (n + 1)
            where cw = safe_wcwidth (TL.head t)
        clip_for_char_width _ _ _ = error "clip_for_char_width applied to undefined"
    in txt''

add_unclipped_text :: DisplayText -> BlitM s ()
add_unclipped_text txt = do
    let op = TextSpan used_display_columns
                      (fromIntegral $ TL.length txt)
                      (T.encodeUtf8 $ TL.toStrict txt)
        used_display_columns = wcswidth $ TL.unpack txt
    use row_offset >>= snoc_op op

-- todo: If there is no background pattern defined then skip to next line.
-- todo: assumes background character is 1 column
add_row_completion :: DisplayRegion -> Int -> BlitM s ()
add_row_completion display_region row = do
    all_row_ops <- view mrow_ops
    Background c a <- view bg
    row_ops <- lift $ lift $ Vector.read all_row_ops row
    let end_x = span_ops_effected_columns row_ops
    when (end_x < region_width display_region) $ do
        let ow = region_width display_region - end_x
            op = TextSpan ow ow (T.encodeUtf8 $ T.replicate (fromIntegral ow) (T.singleton c))
        snoc_op (AttributeChange a) row
        snoc_op op row

-- | snocs the operation to the operations for the given row.
snoc_op :: SpanOp -> Int -> BlitM s ()
snoc_op !op !row = do
    the_mrow_ops <- view mrow_ops
    lift $ lift $ do
        ops <- Vector.read the_mrow_ops row
        let ops' = Vector.snoc ops op
        Vector.write the_mrow_ops row ops'

