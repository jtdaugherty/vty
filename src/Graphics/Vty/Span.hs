-- Copyright Corey O'Connor
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ApplicativeDo #-}
-- | A picture is translated into a sequences of state changes and
-- character spans. The attribute is applied to all following spans,
-- including spans of the next row. The nth element of the sequence
-- represents the nth row (from top to bottom) of the picture to render.
--
-- A span op sequence will be defined for all rows and columns (and no
-- more) of the region provided with the picture to 'spansForPic'.
module Graphics.Vty.Span where

import Graphics.Vty.Attributes (Attr)
import Graphics.Vty.Image
import Graphics.Vty.Image.Internal ( clipText )

import qualified Data.Text.Lazy as TL
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad.ST.Strict
import Data.Vector.Mutable
import Control.Monad.Reader
import qualified Data.Vector.Mutable as MV
import Data.Functor
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.STRef
import Control.Monad.Writer.Strict

-- | This represents an operation on the terminal: either an attribute
-- change or the output of a text string.
data SpanOp =
    -- | A span of UTF-8 text occupies a specific number of screen space
    -- columns. A single UTF character does not necessarily represent 1
    -- colunm. See Codec.Binary.UTF8.Width TextSpan [Attr] [output width
    -- in columns] [number of characters] [data]
      TextSpan
      { textSpanAttr :: !Attr
      , textSpanOutputWidth :: !Int
      , textSpanCharWidth :: !Int
      , textSpanText :: DisplayText
      }
    -- | Skips the given number of columns.
    | Skip !Int
    -- | Marks the end of a row. Specifies how many columns are
    -- remaining. These columns will not be explicitly overwritten with
    -- the span ops. The terminal is require to assure the remaining
    -- columns are clear.
    | RowEnd !Int
    deriving Eq

-- | A vector of span operations executed in succession. This represents
-- the operations required to render a row of the terminal. The
-- operations in one row may affect subsequent rows. For example,
-- setting the foreground color in one row will affect all subsequent
-- rows until the foreground color is changed.
type SpanOps = Vector SpanOp

dropOps :: Int -> SpanOps -> SpanOps
dropOps n initOps =
    let (edgeShard, Sum opsTaken) = runWriter $ dropOps' n initOps
        opsLeft = V.unsafeDrop opsTaken initOps
        secondOps = case edgeShard of
            Nothing -> opsLeft
            Just op -> V.cons op opsLeft
    in secondOps
    where
        -- | Just means that the op on the edge was split and the returned value should be on the right.
        dropOps' :: Int -> SpanOps -> Writer (Sum Int) (Maybe SpanOp)
        dropOps' 0 _ = pure Nothing
        dropOps' remainingColumns ops = tell (Sum 1) *> case Vector.unsafeHead ops of
            t@TextSpan {} -> if remainingColumns >= textSpanOutputWidth t
                then dropOps' (remainingColumns - textSpanOutputWidth t) (Vector.unsafeTail ops)
                else
                    let postWidth = textSpanOutputWidth t - remainingColumns
                        postTxt = clipText (textSpanText t) remainingColumns postWidth
                        postOp = TextSpan { textSpanAttr = textSpanAttr t
                                            , textSpanOutputWidth = postWidth
                                            , textSpanCharWidth = fromIntegral $! TL.length postTxt
                                            , textSpanText = postTxt
                                            }
                     in pure (Just postOp)
            Skip w -> if remainingColumns >= w
                then dropOps' (remainingColumns - w) (Vector.tail ops)
                else pure (Just $ Skip (w - remainingColumns))
            RowEnd _ -> error "cannot split ops containing a row end"
-- let ((edgeShard, opsTaken), firstOps) =
splitOpsAt :: Int -> SpanOps -> ST s (MVector s SpanOp, SpanOps)
splitOpsAt n initOps = do
        let l = V.length initOps
        vm <- unsafeNew l
        iRef <- newSTRef l
        edgeShard <- runReaderT (splitOpsAt' n initOps) (vm, iRef)
        firstOpIndex <- readSTRef iRef
        let opsTaken = l - firstOpIndex
        let notTaken = V.unsafeDrop opsTaken initOps
        let secondOps = case edgeShard of
                Nothing -> notTaken
                Just op -> V.cons op notTaken
        pure (VM.unsafeDrop firstOpIndex vm, secondOps)
    where
        -- | Prepends the op to the mutable vector
        writeOp :: SpanOp -> ReaderT (MVector s SpanOp, STRef s Int) (ST s) ()
        writeOp op = ReaderT $ \(v, iRef) -> do
            i <- pred <$> readSTRef iRef
            writeSTRef iRef i
            MV.unsafeWrite v i op
            pure ()
        -- | Just means that the op on the edge was split and the returned value should be on the right.
        splitOpsAt' :: Int -> SpanOps -> ReaderT (MVector s SpanOp, STRef s Int) (ST s) (Maybe SpanOp)
        splitOpsAt' 0 _ = pure Nothing
        splitOpsAt' remainingColumns ops = case Vector.unsafeHead ops of
            t@TextSpan {} -> if remainingColumns >= textSpanOutputWidth t
                then splitOpsAt' (remainingColumns - textSpanOutputWidth t) (Vector.unsafeTail ops) <* writeOp t
                else do
                        let preTxt = clipText (textSpanText t) 0 remainingColumns
                            preOp = TextSpan { textSpanAttr = textSpanAttr t
                                            , textSpanOutputWidth = remainingColumns
                                            , textSpanCharWidth = fromIntegral $! TL.length preTxt
                                            , textSpanText = preTxt
                                            }
                            postWidth = textSpanOutputWidth t - remainingColumns
                            postTxt = clipText (textSpanText t) remainingColumns postWidth
                            postOp = TextSpan { textSpanAttr = textSpanAttr t
                                                , textSpanOutputWidth = postWidth
                                                , textSpanCharWidth = fromIntegral $! TL.length postTxt
                                                , textSpanText = postTxt
                                                }
                        writeOp preOp
                        pure $ Just postOp
            Skip w -> if remainingColumns >= w
                then splitOpsAt' (remainingColumns - w) (Vector.tail ops) <* writeOp (Skip w)
                else writeOp (Skip remainingColumns) $> Just (Skip (w - remainingColumns))
            RowEnd _ -> error "cannot split ops containing a row end"

-- | A vector of span operation vectors for display, one per row of the
-- output region.
type DisplayOps = Vector SpanOps

instance Show SpanOp where
    show (TextSpan attr ow cw _) = "TextSpan(" ++ show attr ++ ")(" ++ show ow ++ ", " ++ show cw ++ ")"
    show (Skip ow) = "Skip(" ++ show ow ++ ")"
    show (RowEnd ow) = "RowEnd(" ++ show ow ++ ")"

-- | The number of columns the DisplayOps are defined for.
--
-- All spans are verified to define same number of columns.
displayOpsColumns :: DisplayOps -> Int
displayOpsColumns ops
    | Vector.length ops == 0 = 0
    | otherwise              = Vector.length $ Vector.head ops

-- | The number of rows the DisplayOps are defined for.
displayOpsRows :: DisplayOps -> Int
displayOpsRows = Vector.length

affectedRegion :: DisplayOps -> DisplayRegion
affectedRegion ops = (displayOpsColumns ops, displayOpsRows ops)

-- | The number of columns a SpanOps affects.
spanOpsAffectedColumns :: SpanOps -> Int
spanOpsAffectedColumns inOps = Vector.foldl' spanOpsAffectedColumns' 0 inOps
    where
        spanOpsAffectedColumns' t (TextSpan _ w _ _ ) = t + w
        spanOpsAffectedColumns' t (Skip w) = t + w
        spanOpsAffectedColumns' t (RowEnd w) = t + w

-- | The width of a single SpanOp in columns.
spanOpHasWidth :: SpanOp -> Maybe (Int, Int)
spanOpHasWidth (TextSpan _ ow cw _) = Just (cw, ow)
spanOpHasWidth (Skip ow) = Just (ow,ow)
spanOpHasWidth (RowEnd ow) = Just (ow,ow)

-- | The number of columns to the character at the given position in the
-- span op.
columnsToCharOffset :: Int -> SpanOp -> Int
columnsToCharOffset cx (TextSpan _ _ _ utf8Str) =
    wctlwidth (TL.take (fromIntegral cx) utf8Str)
columnsToCharOffset cx (Skip _) = cx
columnsToCharOffset cx (RowEnd _) = cx
