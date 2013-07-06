-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Vty.Debug ( module Graphics.Vty.Debug
                          , module Graphics.Vty.Debug.Image
                          )
where

import Graphics.Vty.Attributes
import Graphics.Vty.Debug.Image
import Graphics.Vty.Span
import Graphics.Vty.DisplayRegion

import qualified Data.Vector as Vector 

row_ops_effected_columns :: DisplayOps -> [Int]
row_ops_effected_columns spans 
    = Vector.toList $ Vector.map span_ops_effected_columns $ display_ops spans

all_spans_have_width :: DisplayOps -> Int -> Bool
all_spans_have_width spans expected
    = all (== expected) $ Vector.toList $ Vector.map span_ops_effected_columns $ display_ops spans

span_ops_effected_rows :: DisplayOps -> Int
span_ops_effected_rows (DisplayOps _ the_row_ops) 
    = toEnum $ length (filter (not . null . Vector.toList) (Vector.toList the_row_ops))
        
type SpanConstructLog = [SpanConstructEvent]
data SpanConstructEvent = SpanSetAttr Attr

is_set_attr :: Attr -> SpanConstructEvent -> Bool
is_set_attr expected_attr (SpanSetAttr in_attr)
    | in_attr == expected_attr = True
is_set_attr _attr _event = False

data MockWindow = MockWindow Int Int
    deriving (Show, Eq)

region_for_window :: MockWindow -> DisplayRegion
region_for_window (MockWindow w h) = DisplayRegion w h

