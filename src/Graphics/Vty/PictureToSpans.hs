-- Copyright Corey O'Connor<coreyoconnor@gmail.com>
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
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

#if __GLASGOW_HASKELL__ < 708
import Control.Monad.ST.Strict hiding ( unsafeIOToST )
#else
import Control.Monad.ST.Strict hiding ( unsafeIOToST )
#endif

import qualified Data.Vector as Vector hiding ( take, replicate )
import Data.Monoid (mappend)
import Data.Vector.Mutable ( MVector(..))
import qualified Data.Vector.Mutable as MVector

import qualified Data.Text.Lazy as TL

type MRowOps s = MVector s SpanOps

type MSpanOps s = MVector s SpanOp

-- transform plus clip. More or less.
data BlitState = BlitState
    -- we always snoc to the operation vectors. Thus the columnOffset = length of row at rowOffset
    -- although, one possibility is to merge layers right in snocOp (naming it something else, of
    -- course). In which case columnnOffset would be applicable.
    -- Right now we need it to exist.
    { _columnOffset :: Int
    , _rowOffset :: Int
    -- clip coordinate space is in image space. Which means it's >= 0 and < imageWidth.
    , _skipColumns :: Int
    -- >= 0 and < imageHeight
    , _skipRows :: Int
    -- includes consideration of skipColumns. In display space.
    -- The number of columns from the next column to be defined to the end of the display for the
    -- row.
    , _remainingColumns :: Int
    -- includes consideration of skipRows. In display space.
    , _remainingRows :: Int
    }

makeLenses ''BlitState

data BlitEnv s = BlitEnv
    { _region :: DisplayRegion
    , _mrowOps :: MRowOps s
    }

makeLenses ''BlitEnv

type BlitM s a = ReaderT (BlitEnv s) (StateT BlitState (ST s)) a

-- | Produces the span ops that will render the given picture, possibly cropped or padded, into the
-- specified region.
displayOpsForPic :: Picture -> DisplayRegion -> DisplayOps
displayOpsForPic pic r = Vector.create (combinedOpsForLayers pic r)

-- | Returns the DisplayOps for an image rendered to a window the size of the image.
--
-- largerly used only for debugging.
displayOpsForImage :: Image -> DisplayOps
displayOpsForImage i = displayOpsForPic (picForImage i) (imageWidth i, imageHeight i)

-- | Produces the span ops for each layer then combines them.
--
-- TODO: a fold over a builder function. start with span ops that are a bg fill of the entire
-- region.
combinedOpsForLayers :: Picture -> DisplayRegion -> ST s (MRowOps s)
combinedOpsForLayers pic r
    | regionWidth r == 0 || regionHeight r == 0 = MVector.new 0
    | otherwise = do
        layerOps <- mapM (\layer -> buildSpans layer r) (picLayers pic)
        case layerOps of
            []    -> fail "empty picture"
            [ops] -> substituteSkips (picBackground pic) ops
            -- instead of merging ops after generation the merging can be performed as part of
            -- snocOp.
            topOps : lowerOps -> do
                ops <- foldM mergeUnder topOps lowerOps
                substituteSkips (picBackground pic) ops

substituteSkips :: Background -> MRowOps s -> ST s (MRowOps s)
substituteSkips ClearBackground ops = do
    forM_ [0 .. MVector.length ops - 1] $ \row -> do
        rowOps <- MVector.read ops row
        -- the image operations assure that background fills are combined.
        -- clipping a background fill does not split the background fill.
        -- merging of image layers can split a skip, but only by the insertion of a non skip.
        -- all this combines to mean we can check the last operation and remove it if it's a skip
        -- todo: or does it?
        let rowOps' = case Vector.last rowOps of
                        Skip w -> Vector.init rowOps `Vector.snoc` RowEnd w
                        _      -> rowOps
        -- now all the skips can be replaced by replications of ' ' of the required width.
        let rowOps'' = swapSkipsForSingleColumnCharSpan ' ' currentAttr rowOps'
        MVector.write ops row rowOps''
    return ops
substituteSkips (Background {backgroundChar, backgroundAttr}) ops = do
    -- At this point we decide if the background character is single column or not.
    -- obviously, single column is easier.
    case safeWcwidth backgroundChar of
        w | w == 0 -> fail $ "invalid background character " ++ show backgroundChar
          | w == 1 -> do
                forM_ [0 .. MVector.length ops - 1] $ \row -> do
                    rowOps <- MVector.read ops row
                    let rowOps' = swapSkipsForSingleColumnCharSpan backgroundChar backgroundAttr rowOps
                    MVector.write ops row rowOps'
          | otherwise -> do
                forM_ [0 .. MVector.length ops - 1] $ \row -> do
                    rowOps <- MVector.read ops row
                    let rowOps' = swapSkipsForCharSpan w backgroundChar backgroundAttr rowOps
                    MVector.write ops row rowOps'
    return ops

mergeUnder :: MRowOps s -> MRowOps s -> ST s (MRowOps s)
mergeUnder upper lower = do
    forM_ [0 .. MVector.length upper - 1] $ \row -> do
        upperRowOps <- MVector.read upper row
        lowerRowOps <- MVector.read lower row
        let rowOps = mergeRowUnder upperRowOps lowerRowOps
        MVector.write upper row rowOps
    return upper

-- fugly
mergeRowUnder :: SpanOps -> SpanOps -> SpanOps
mergeRowUnder upperRowOps lowerRowOps =
    onUpperOp Vector.empty (Vector.head upperRowOps) (Vector.tail upperRowOps) lowerRowOps
    where
        -- H: it will never be the case that we are out of upper ops before lower ops.
        onUpperOp :: SpanOps -> SpanOp -> SpanOps -> SpanOps -> SpanOps
        onUpperOp outOps op@(TextSpan _ w _ _) upperOps lowerOps =
            let lowerOps' = dropOps w lowerOps
                outOps' = Vector.snoc outOps op
            in if Vector.null lowerOps'
                then outOps'
                else onUpperOp outOps' (Vector.head upperOps) (Vector.tail upperOps) lowerOps'
        onUpperOp outOps (Skip w) upperOps lowerOps =
            let (ops', lowerOps') = splitOpsAt w lowerOps
                outOps' = outOps `mappend` ops'
            in if Vector.null lowerOps'
                then outOps'
                else onUpperOp outOps' (Vector.head upperOps) (Vector.tail upperOps) lowerOps'
        onUpperOp _ (RowEnd _) _ _ = error "cannot merge rows containing RowEnd ops"


swapSkipsForSingleColumnCharSpan :: Char -> Attr -> SpanOps -> SpanOps
swapSkipsForSingleColumnCharSpan c a = Vector.map f
    where f (Skip ow) = let txt = TL.pack $ replicate ow c
                        in TextSpan a ow ow txt
          f v = v

swapSkipsForCharSpan :: Int -> Char -> Attr -> SpanOps -> SpanOps
swapSkipsForCharSpan w c a = Vector.map f
    where
        f (Skip ow) = let txt0Cw = ow `div` w
                          txt0 = TL.pack $ replicate txt0Cw c
                          txt1Cw = ow `mod` w
                          txt1 = TL.pack $ replicate txt1Cw '…'
                          cw = txt0Cw + txt1Cw
                          txt = txt0 `TL.append` txt1
                      in TextSpan a ow cw txt
        f v = v

-- | Builds a vector of row operations that will output the given picture to the terminal.
--
-- Crops to the given display region.
--
-- \todo I'm pretty sure there is an algorithm that does not require a mutable buffer.
buildSpans :: Image -> DisplayRegion -> ST s (MRowOps s)
buildSpans image outRegion = do
    -- First we create a mutable vector for each rows output operations.
    outOps <- MVector.replicate (regionHeight outRegion) Vector.empty
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
    when (regionHeight outRegion > 0 && regionWidth outRegion > 0) $ do
        -- The ops builder recursively descends the image and outputs span ops that would
        -- display that image. The number of columns remaining in this row before exceeding the
        -- bounds is also provided. This is used to clip the span ops produced to the display.
        let fullBuild = do
                startImageBuild image
                -- Fill in any unspecified columns with a skip.
                forM_ [0 .. (regionHeight outRegion - 1)] (addRowCompletion outRegion)
            initEnv   = BlitEnv outRegion outOps
            initState = BlitState 0 0 0 0 (regionWidth outRegion) (regionHeight outRegion)
        _ <- runStateT (runReaderT fullBuild initEnv) initState
        return ()
    return outOps

-- | Add the operations required to build a given image to the current set of row operations
-- returns the number of columns and rows contributed to the output.
startImageBuild :: Image -> BlitM s ()
startImageBuild image = do
    outOfBounds <- isOutOfBounds image <$> get
    when (not outOfBounds) $ addMaybeClipped image

isOutOfBounds :: Image -> BlitState -> Bool
isOutOfBounds i s
    | s ^. remainingColumns <= 0              = True
    | s ^. remainingRows    <= 0              = True
    | s ^. skipColumns      >= imageWidth i  = True
    | s ^. skipRows         >= imageHeight i = True
    | otherwise = False

-- | This adds an image that might be partially clipped to the output ops.
--
-- This is a very touchy algorithm. Too touchy. For instance, the CropRight and CropBottom
-- implementations are odd. They pass the current tests but something seems terribly wrong about all
-- this.
--
-- \todo prove this cannot be called in an out of bounds case.
addMaybeClipped :: forall s . Image -> BlitM s ()
addMaybeClipped EmptyImage = return ()
addMaybeClipped (HorizText a textStr ow _cw) = do
    -- TODO: assumption that text spans are only 1 row high.
    s <- use skipRows
    when (s < 1) $ do
        leftClip <- use skipColumns
        rightClip <- use remainingColumns
        let leftClipped = leftClip > 0
            rightClipped = (ow - leftClip) > rightClip
        if leftClipped || rightClipped
            then let textStr' = clipText textStr leftClip rightClip
                 in addUnclippedText a textStr'
            else addUnclippedText a textStr
addMaybeClipped (VertJoin topImage bottomImage _ow oh) = do
    addMaybeClippedJoin "vert_join" skipRows remainingRows rowOffset
                           (imageHeight topImage)
                           topImage
                           bottomImage
                           oh
addMaybeClipped (HorizJoin leftImage rightImage ow _oh) = do
    addMaybeClippedJoin "horiz_join" skipColumns remainingColumns columnOffset
                           (imageWidth leftImage)
                           leftImage
                           rightImage
                           ow
addMaybeClipped BGFill {outputWidth, outputHeight} = do
    s <- get
    let outputWidth'  = min (outputWidth  - s^.skipColumns) (s^.remainingColumns)
        outputHeight' = min (outputHeight - s^.skipRows   ) (s^.remainingRows)
    y <- use rowOffset
    forM_ [y..y+outputHeight'-1] $ snocOp (Skip outputWidth')
addMaybeClipped CropRight {croppedImage, outputWidth} = do
    s <- use skipColumns
    r <- use remainingColumns
    let x = outputWidth - s
    when (x < r) $ remainingColumns .= x
    addMaybeClipped croppedImage
addMaybeClipped CropLeft {croppedImage, leftSkip} = do
    skipColumns += leftSkip
    addMaybeClipped croppedImage
addMaybeClipped CropBottom {croppedImage, outputHeight} = do
    s <- use skipRows
    r <- use remainingRows
    let x = outputHeight - s
    when (x < r) $ remainingRows .= x
    addMaybeClipped croppedImage
addMaybeClipped CropTop {croppedImage, topSkip} = do
    skipRows += topSkip
    addMaybeClipped croppedImage

addMaybeClippedJoin :: forall s . String 
                       -> Lens BlitState BlitState Int Int
                       -> Lens BlitState BlitState Int Int
                       -> Lens BlitState BlitState Int Int
                       -> Int
                       -> Image
                       -> Image
                       -> Int
                       -> BlitM s ()
addMaybeClippedJoin name skip remaining offset i0Dim i0 i1 size = do
    state <- get
    when (state^.remaining <= 0) $ fail $ name ++ " with remaining <= 0"
    case state^.skip of
        s -- TODO: check if clipped in other dim. if not use addUnclipped
          | s >= size -> fail $ name ++ " on fully clipped"
          | s == 0    -> if state^.remaining > i0Dim 
                            then do
                                addMaybeClipped i0
                                put $ state & offset +~ i0Dim & remaining -~ i0Dim
                                addMaybeClipped i1
                            else addMaybeClipped i0
          | s < i0Dim  ->
                let i0Dim' = i0Dim - s
                in if state^.remaining <= i0Dim'
                    then addMaybeClipped i0
                    else do
                        addMaybeClipped i0
                        put $ state & offset +~ i0Dim' & remaining -~ i0Dim' & skip .~ 0
                        addMaybeClipped i1
          | s >= i0Dim -> do
                put $ state & skip -~ i0Dim
                addMaybeClipped i1
        _ -> fail $ name ++ " has unhandled skip class"

addUnclippedText :: Attr -> DisplayText -> BlitM s ()
addUnclippedText a txt = do
    let op = TextSpan a usedDisplayColumns
                      (fromIntegral $ TL.length txt)
                      txt
        usedDisplayColumns = wcswidth $ TL.unpack txt
    use rowOffset >>= snocOp op

addRowCompletion :: DisplayRegion -> Int -> BlitM s ()
addRowCompletion displayRegion row = do
    allRowOps <- view mrowOps
    rowOps <- lift $ lift $ MVector.read allRowOps row
    let endX = spanOpsEffectedColumns rowOps
    when (endX < regionWidth displayRegion) $ do
        let ow = regionWidth displayRegion - endX
        snocOp (Skip ow) row

-- | snocs the operation to the operations for the given row.
snocOp :: SpanOp -> Int -> BlitM s ()
snocOp !op !row = do
    theMrowOps <- view mrowOps
    theRegion <- view region
    lift $ lift $ do
        ops <- MVector.read theMrowOps row
        let ops' = Vector.snoc ops op
        when (spanOpsEffectedColumns ops' > regionWidth theRegion)
             $ fail $ "row " ++ show row ++ " now exceeds region width"
        MVector.write theMrowOps row ops'
