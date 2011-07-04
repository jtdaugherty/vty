-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Vty.Debug ( module Graphics.Vty.Debug
                          , module Graphics.Vty.Debug.Image
                          )
where

import Graphics.Vty.Attributes
import Graphics.Vty.Debug.Image
import Graphics.Vty.Picture
import Graphics.Vty.Span
import Graphics.Vty.DisplayRegion

import Data.Array
import Data.Word

row_ops_effected_columns :: SpanOpSequence -> [Word]
row_ops_effected_columns spans 
    = map span_ops_effected_columns (elems $ row_ops spans)

all_spans_have_width :: SpanOpSequence -> Word -> Bool
all_spans_have_width spans expected
    = all (== expected) ( map span_ops_effected_columns (elems $ row_ops spans) )

span_ops_effected_rows :: SpanOpSequence -> Word
span_ops_effected_rows (SpanOpSequence _ the_row_ops) 
    = toEnum $ length (filter (not . null) (elems the_row_ops))
        
type SpanConstructLog = [SpanConstructEvent]
data SpanConstructEvent = SpanSetAttr Attr

is_set_attr :: Attr -> SpanConstructEvent -> Bool
is_set_attr expected_attr (SpanSetAttr in_attr)
    | in_attr == expected_attr = True
is_set_attr _attr _event = False

data DebugWindow = DebugWindow Word Word
    deriving (Show, Eq)

region_for_window :: DebugWindow -> DisplayRegion
region_for_window (DebugWindow w h) = DisplayRegion w h

type TestWindow = DebugWindow

