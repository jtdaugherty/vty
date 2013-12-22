-- Copyright Corey O'Connor<coreyoconnor@gmail.com>
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{- | Transforms an image into rows of operations.
 -}
module Graphics.Vty.PictureToSpans where

import Graphics.Vty.Prelude

import Graphics.Vty.Image
import Graphics.Vty.Image.Internal
import Graphics.Vty.Picture
import Graphics.Vty.Span

import Control.Lens hiding ( op )
import Control.Monad.Reader
import Control.Monad.State.Strict hiding ( state )
import Control.Monad.ST.Strict hiding ( unsafeIOToST )

import qualified Data.Vector as Vector hiding ( take, replicate )
import Data.Monoid (mappend)
import Data.Vector.Mutable ( MVector(..))
import qualified Data.Vector.Mutable as MVector

import qualified Data.Text.Lazy as TL

type MRowOps s = MVector s SpanOps

type MSpanOps s = MVector s SpanOp

-- transform plus clip. More or less.
data BlitState = BlitState
    -- we always snoc to the operation vectors. Thus the column_offset = length of row at row_offset
    -- although, one possibility is to merge layers right in snoc_op (naming it something else, of
    -- course). In which case columnn_offset would be applicable.
    -- Right now we need it to exist.
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
    { _region :: DisplayRegion
    , _mrow_ops :: MRowOps s
    }

makeLenses ''BlitEnv

type BlitM s a = ReaderT (BlitEnv s) (StateT BlitState (ST s)) a

-- | Produces the span ops that will render the given picture, possibly cropped or padded, into the
-- specified region.
display_ops_for_pic :: Picture -> DisplayRegion -> DisplayOps
display_ops_for_pic pic r = Vector.create (combined_ops_for_layers pic r)

-- | Returns the DisplayOps for an image rendered to a window the size of the image.
--
-- largerly used only for debugging.
display_ops_for_image :: Image -> DisplayOps
display_ops_for_image i = display_ops_for_pic (pic_for_image i) (image_width i, image_height i)

-- | Produces the span ops for each layer then combines them.
--
-- TODO: a fold over a builder function. start with span ops that are a bg fill of the entire
-- region.
combined_ops_for_layers :: Picture -> DisplayRegion -> ST s (MRowOps s)
combined_ops_for_layers pic r
    | region_width r == 0 || region_height r == 0 = MVector.new 0
    | otherwise = do
        layer_ops <- mapM (\layer -> build_spans layer r) (pic_layers pic)
        case layer_ops of
            []    -> fail "empty picture"
            [ops] -> substitute_skips (pic_background pic) ops
            -- instead of merging ops after generation the merging can be performed as part of
            -- snoc_op.
            top_ops : lower_ops -> do
                ops <- foldM merge_under top_ops lower_ops
                substitute_skips (pic_background pic) ops

substitute_skips :: Background -> MRowOps s -> ST s (MRowOps s)
substitute_skips ClearBackground ops = do
    forM_ [0 .. MVector.length ops - 1] $ \row -> do
        row_ops <- MVector.read ops row
        -- the image operations assure that background fills are combined.
        -- clipping a background fill does not split the background fill.
        -- merging of image layers can split a skip, but only by the insertion of a non skip.
        -- all this combines to mean we can check the last operation and remove it if it's a skip
        -- todo: or does it?
        let row_ops' = case Vector.last row_ops of
                        Skip w -> Vector.init row_ops `Vector.snoc` RowEnd w
                        _      -> row_ops
        -- now all the skips can be replaced by replications of ' ' of the required width.
        let row_ops'' = swap_skips_for_single_column_char_span ' ' current_attr row_ops'
        MVector.write ops row row_ops''
    return ops
substitute_skips (Background {background_char, background_attr}) ops = do
    -- At this point we decide if the background character is single column or not.
    -- obviously, single column is easier.
    case safe_wcwidth background_char of
        w | w == 0 -> fail $ "invalid background character " ++ show background_char
          | w == 1 -> do
                forM_ [0 .. MVector.length ops - 1] $ \row -> do
                    row_ops <- MVector.read ops row
                    let row_ops' = swap_skips_for_single_column_char_span background_char background_attr row_ops
                    MVector.write ops row row_ops'
          | otherwise -> do
                forM_ [0 .. MVector.length ops - 1] $ \row -> do
                    row_ops <- MVector.read ops row
                    let row_ops' = swap_skips_for_char_span w background_char background_attr row_ops
                    MVector.write ops row row_ops'
    return ops

merge_under :: MRowOps s -> MRowOps s -> ST s (MRowOps s)
merge_under upper lower = do
    forM_ [0 .. MVector.length upper - 1] $ \row -> do
        upper_row_ops <- MVector.read upper row
        lower_row_ops <- MVector.read lower row
        let row_ops = merge_row_under upper_row_ops lower_row_ops
        MVector.write upper row row_ops
    return upper

-- fugly
merge_row_under :: SpanOps -> SpanOps -> SpanOps
merge_row_under upper_row_ops lower_row_ops =
    on_upper_op Vector.empty (Vector.head upper_row_ops) (Vector.tail upper_row_ops) lower_row_ops
    where
        -- H: it will never be the case that we are out of upper ops before lower ops.
        on_upper_op :: SpanOps -> SpanOp -> SpanOps -> SpanOps -> SpanOps
        on_upper_op out_ops op@(TextSpan _ w _ _) upper_ops lower_ops =
            let lower_ops' = drop_ops w lower_ops
                out_ops' = Vector.snoc out_ops op
            in if Vector.null lower_ops'
                then out_ops'
                else on_upper_op out_ops' (Vector.head upper_ops) (Vector.tail upper_ops) lower_ops'
        on_upper_op out_ops (Skip w) upper_ops lower_ops =
            let (ops', lower_ops') = split_ops_at w lower_ops
                out_ops' = out_ops `mappend` ops'
            in if Vector.null lower_ops'
                then out_ops'
                else on_upper_op out_ops' (Vector.head upper_ops) (Vector.tail upper_ops) lower_ops'
        on_upper_op _ (RowEnd _) _ _ = error "cannot merge rows containing RowEnd ops"


swap_skips_for_single_column_char_span :: Char -> Attr -> SpanOps -> SpanOps
swap_skips_for_single_column_char_span c a = Vector.map f
    where f (Skip ow) = let txt = TL.pack $ replicate ow c
                        in TextSpan a ow ow txt
          f v = v

swap_skips_for_char_span :: Int -> Char -> Attr -> SpanOps -> SpanOps
swap_skips_for_char_span w c a = Vector.map f
    where
        f (Skip ow) = let txt_0_cw = ow `div` w
                          txt_0 = TL.pack $ replicate txt_0_cw c
                          txt_1_cw = ow `mod` w
                          txt_1 = TL.pack $ replicate txt_1_cw '…'
                          cw = txt_0_cw + txt_1_cw
                          txt = txt_0 `TL.append` txt_1
                      in TextSpan a ow cw txt
        f v = v

-- | Builds a vector of row operations that will output the given picture to the terminal.
--
-- Crops to the given display region.
--
-- \todo I'm pretty sure there is an algorithm that does not require a mutable buffer.
build_spans :: Image -> DisplayRegion -> ST s (MRowOps s)
build_spans image out_region = do
    -- First we create a mutable vector for each rows output operations.
    out_ops <- MVector.replicate (region_height out_region) Vector.empty
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
                -- Fill in any unspecified columns with a skip.
                forM_ [0 .. (region_height out_region - 1)] (add_row_completion out_region)
            init_env   = BlitEnv out_region out_ops
            init_state = BlitState 0 0 0 0 (region_width out_region) (region_height out_region)
        _ <- runStateT (runReaderT full_build init_env) init_state
        return ()
    return out_ops

-- | Add the operations required to build a given image to the current set of row operations
-- returns the number of columns and rows contributed to the output.
start_image_build :: Image -> BlitM s ()
start_image_build image = do
    out_of_bounds <- is_out_of_bounds image <$> get
    when (not out_of_bounds) $ add_maybe_clipped image

is_out_of_bounds :: Image -> BlitState -> Bool
is_out_of_bounds i s
    | s ^. remaining_columns <= 0              = True
    | s ^. remaining_rows    <= 0              = True
    | s ^. skip_columns      >= image_width i  = True
    | s ^. skip_rows         >= image_height i = True
    | otherwise = False

-- | This adds an image that might be partially clipped to the output ops.
--
-- This is a very touchy algorithm. Too touchy. For instance, the CropRight and CropBottom
-- implementations are odd. They pass the current tests but something seems terribly wrong about all
-- this.
--
-- \todo prove this cannot be called in an out of bounds case.
add_maybe_clipped :: forall s . Image -> BlitM s ()
add_maybe_clipped EmptyImage = return ()
add_maybe_clipped (HorizText a text_str ow _cw) = do
    -- TODO: assumption that text spans are only 1 row high.
    s <- use skip_rows
    when (s < 1) $ do
        left_clip <- use skip_columns
        right_clip <- use remaining_columns
        let left_clipped = left_clip > 0
            right_clipped = (ow - left_clip) > right_clip
        if left_clipped || right_clipped
            then let text_str' = clip_text text_str left_clip right_clip
                 in add_unclipped_text a text_str'
            else add_unclipped_text a text_str
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
    y <- use row_offset
    forM_ [y..y+output_height'-1] $ snoc_op (Skip output_width')
add_maybe_clipped CropRight {cropped_image, output_width} = do
    s <- use skip_columns
    r <- use remaining_columns
    let x = output_width - s
    when (x < r) $ remaining_columns .= x
    add_maybe_clipped cropped_image
add_maybe_clipped CropLeft {cropped_image, left_skip} = do
    skip_columns += left_skip
    add_maybe_clipped cropped_image
add_maybe_clipped CropBottom {cropped_image, output_height} = do
    s <- use skip_rows
    r <- use remaining_rows
    let x = output_height - s
    when (x < r) $ remaining_rows .= x
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
    when (state^.remaining <= 0) $ fail $ name ++ " with remaining <= 0"
    case state^.skip of
        s -- TODO: check if clipped in other dim. if not use add_unclipped
          | s >= size -> fail $ name ++ " on fully clipped"
          | s == 0    -> if state^.remaining > i0_dim 
                            then do
                                add_maybe_clipped i0
                                put $ state & offset +~ i0_dim & remaining -~ i0_dim
                                add_maybe_clipped i1
                            else add_maybe_clipped i0
          | s < i0_dim  ->
                let i0_dim' = i0_dim - s
                in if state^.remaining <= i0_dim'
                    then add_maybe_clipped i0
                    else do
                        add_maybe_clipped i0
                        put $ state & offset +~ i0_dim' & remaining -~ i0_dim' & skip .~ 0
                        add_maybe_clipped i1
          | s >= i0_dim -> do
                put $ state & skip -~ i0_dim
                add_maybe_clipped i1
        _ -> fail $ name ++ " has unhandled skip class"

add_unclipped_text :: Attr -> DisplayText -> BlitM s ()
add_unclipped_text a txt = do
    let op = TextSpan a used_display_columns
                      (fromIntegral $ TL.length txt)
                      txt
        used_display_columns = wcswidth $ TL.unpack txt
    use row_offset >>= snoc_op op

add_row_completion :: DisplayRegion -> Int -> BlitM s ()
add_row_completion display_region row = do
    all_row_ops <- view mrow_ops
    row_ops <- lift $ lift $ MVector.read all_row_ops row
    let end_x = span_ops_effected_columns row_ops
    when (end_x < region_width display_region) $ do
        let ow = region_width display_region - end_x
        snoc_op (Skip ow) row

-- | snocs the operation to the operations for the given row.
snoc_op :: SpanOp -> Int -> BlitM s ()
snoc_op !op !row = do
    the_mrow_ops <- view mrow_ops
    the_region <- view region
    lift $ lift $ do
        ops <- MVector.read the_mrow_ops row
        let ops' = Vector.snoc ops op
        when (span_ops_effected_columns ops' > region_width the_region)
             $ fail $ "row " ++ show row ++ " now exceeds region width"
        MVector.write the_mrow_ops row ops'
