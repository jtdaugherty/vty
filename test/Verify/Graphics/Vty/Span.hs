{-# LANGUAGE FlexibleInstances #-}
module Verify.Graphics.Vty.Span
  ( module Verify.Graphics.Vty.Span
  , module Graphics.Vty.Span
  )
where

import Graphics.Vty.Debug
import Graphics.Vty.Span

import Verify.Graphics.Vty.Picture

import qualified Data.Vector as Vector
import Data.Word

import Verify

isAttrSpanOp :: SpanOp -> Bool
isAttrSpanOp TextSpan {} = True
isAttrSpanOp _           = False

verifyAllSpansHaveWidth i spans w
    = let v = map (\s -> (spanOpsAffectedColumns s /= w, s)) (Vector.toList spans)
      in case any ((== True) . fst) v of
        False -> succeeded
        True -> failed { reason = "Not all spans contained operations defining exactly "
                                ++ show w
                                ++ " columns of output - \n"
                                ++ (concatMap ((++ "\n") . show)) v
                            }

verifyOpsEquality i_ops i_alt_ops =
    if i_ops == i_alt_ops
        then succeeded
        else failed { reason = "ops for alternate image " ++ show i_alt_ops
                               ++ " are not the same as " ++ show i_ops
                    }
