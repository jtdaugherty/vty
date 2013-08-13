{-# LANGUAGE FlexibleInstances #-}
module Verify.Graphics.Vty.Span ( module Verify.Graphics.Vty.Span
                                , module Graphics.Vty.Span
                                )
    where

import Graphics.Vty.Debug
import Graphics.Vty.Span

import Verify.Graphics.Vty.Picture

import qualified Data.Vector as Vector
import Data.Word

import Verify

is_attr_span_op :: SpanOp -> Bool
is_attr_span_op AttributeChange {} = True
is_attr_span_op _                  = False

verify_all_spans_have_width i spans w
    = let v = map (\s -> (span_ops_effected_columns s /= w, s)) (Vector.toList spans)
      in case any ((== True) . fst) v of
        False -> succeeded
        True -> failed { reason = "Not all spans contained operations defining exactly " 
                                ++ show w
                                ++ " columns of output - \n"
                                ++ (concatMap ((++ "\n") . show)) v
                            }

