{-# LANGUAGE FlexibleInstances #-}
module Verify.Graphics.Vty.Span ( module Verify.Graphics.Vty.Span
                                , module Graphics.Vty.Span
                                )
    where

import Graphics.Vty.Debug
import Graphics.Vty.Span

import Verify.Graphics.Vty.Picture

import Data.Word

import Verify

is_attr_span_op :: SpanOp -> Bool
is_attr_span_op AttributeChange {} = True
is_attr_span_op _                  = False

verify_all_spans_have_width i spans w
    = case all_spans_have_width spans w of
        True -> succeeded
        False -> failed { reason = "Not all spans contained operations defining exactly " 
                                 ++ show w
                                 ++ " columns of output -\n"
                                 ++ show i
                                 ++ "\n->\n"
                                 ++ show spans
                        }
