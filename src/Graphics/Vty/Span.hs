-- Copyright Corey O'Connor
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
-- | A picture is translated into a sequences of state changes and
-- character spans. State changes are currently limited to new attribute
-- values. The attribute is applied to all following spans. Including
-- spans of the next row. The nth element of the sequence represents the
-- nth row (from top to bottom) of the picture to render.
--
-- A span op sequence will be defined for all rows and columns (and no
-- more) of the region provided with the picture to `spansForPic`.
--
-- todo: Partition attribute changes into multiple categories according
-- to the serialized representation of the various attributes.
module Graphics.Vty.Span where

import Graphics.Vty.Attributes (Attr)
import Graphics.Vty.Image
import Graphics.Vty.Image.Internal ( clipText )

import qualified Data.Text.Lazy as TL
import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- | This represents an operation on the terminal. Either an attribute
-- change or the output of a text string.
data SpanOp =
    -- | a span of UTF-8 text occupies a specific number of screen space
    -- columns. A single UTF character does not necessarially represent
    -- 1 colunm. See Codec.Binary.UTF8.Width TextSpan [Attr] [output
    -- width in columns] [number of characters] [data]
      TextSpan
      { textSpanAttr :: !Attr
      , textSpanOutputWidth :: !Int
      , textSpanCharWidth :: !Int
      , textSpanText :: DisplayText
      }
    -- | Skips the given number of columns
    -- A skip is transparent.... maybe? I am not sure how attribute
    -- changes interact.
    -- todo: separate from this type.
    | Skip !Int
    -- | Marks the end of a row. specifies how many columns are
    -- remaining. These columns will not be explicitly overwritten with
    -- the span ops. The terminal is require to assure the remaining
    -- columns are clear.
    -- todo: separate from this type.
    | RowEnd !Int
    deriving Eq

-- | vector of span operations. executed in succession. This represents
-- the operations required to render a row of the terminal. The
-- operations in one row may effect subsequent rows.
-- EG: Setting the foreground color in one row will effect all
-- subsequent rows until the foreground color is changed.
type SpanOps = Vector SpanOp

dropOps :: Int -> SpanOps -> SpanOps
dropOps w = snd . splitOpsAt w

splitOpsAt :: Int -> SpanOps -> (SpanOps, SpanOps)
splitOpsAt inW inOps = splitOpsAt' inW inOps
    where
        splitOpsAt' 0 ops = (Vector.empty, ops)
        splitOpsAt' remainingColumns ops = case Vector.head ops of
            t@(TextSpan {}) -> if remainingColumns >= textSpanOutputWidth t
                then let (pre,post) = splitOpsAt' (remainingColumns - textSpanOutputWidth t)
                                                  (Vector.tail ops)
                     in (Vector.cons t pre, post)
                else let preTxt = clipText (textSpanText t) 0 remainingColumns
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
                     in ( Vector.singleton preOp
                        , Vector.cons postOp (Vector.tail ops)
                        )
            Skip w -> if remainingColumns >= w
                then let (pre,post) = splitOpsAt' (remainingColumns - w) (Vector.tail ops)
                     in (Vector.cons (Skip w) pre, post)
                else ( Vector.singleton $ Skip remainingColumns
                     , Vector.cons (Skip (w - remainingColumns)) (Vector.tail ops)
                     )
            RowEnd _ -> error "cannot split ops containing a row end"

-- | vector of span operation vectors for display. One per row of the
-- output region.
type DisplayOps = Vector SpanOps

instance Show SpanOp where
    show (TextSpan attr ow cw _) = "TextSpan(" ++ show attr ++ ")(" ++ show ow ++ ", " ++ show cw ++ ")"
    show (Skip ow) = "Skip(" ++ show ow ++ ")"
    show (RowEnd ow) = "RowEnd(" ++ show ow ++ ")"

-- | Number of columns the DisplayOps are defined for
--
-- All spans are verified to define same number of columns. See:
-- VerifySpanOps
displayOpsColumns :: DisplayOps -> Int
displayOpsColumns ops
    | Vector.length ops == 0 = 0
    | otherwise              = Vector.length $ Vector.head ops

-- | Number of rows the DisplayOps are defined for
displayOpsRows :: DisplayOps -> Int
displayOpsRows ops = Vector.length ops

effectedRegion :: DisplayOps -> DisplayRegion
effectedRegion ops = (displayOpsColumns ops, displayOpsRows ops)

-- | The number of columns a SpanOps effects.
spanOpsEffectedColumns :: SpanOps -> Int
spanOpsEffectedColumns inOps = Vector.foldl' spanOpsEffectedColumns' 0 inOps
    where
        spanOpsEffectedColumns' t (TextSpan _ w _ _ ) = t + w
        spanOpsEffectedColumns' t (Skip w) = t + w
        spanOpsEffectedColumns' t (RowEnd w) = t + w

-- | The width of a single SpanOp in columns
spanOpHasWidth :: SpanOp -> Maybe (Int, Int)
spanOpHasWidth (TextSpan _ ow cw _) = Just (cw, ow)
spanOpHasWidth (Skip ow) = Just (ow,ow)
spanOpHasWidth (RowEnd ow) = Just (ow,ow)

-- | returns the number of columns to the character at the given
-- position in the span op
columnsToCharOffset :: Int -> SpanOp -> Int
columnsToCharOffset cx (TextSpan _ _ _ utf8Str) =
    let str = TL.unpack utf8Str
    in wcswidth (take cx str)
columnsToCharOffset cx (Skip _) = cx
columnsToCharOffset cx (RowEnd _) = cx
