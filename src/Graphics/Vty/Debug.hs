-- Copyright 2009-2010 Corey O'Connor
module Graphics.Vty.Debug ( module Graphics.Vty.Debug
                          , module Graphics.Vty.Debug.Image
                          )
where

import Graphics.Vty.Prelude

import Graphics.Vty.Attributes
import Graphics.Vty.Debug.Image
import Graphics.Vty.Span

import qualified Data.Vector as Vector

rowOpsEffectedColumns :: DisplayOps -> [Int]
rowOpsEffectedColumns ops
    = Vector.toList $ Vector.map spanOpsEffectedColumns ops

allSpansHaveWidth :: DisplayOps -> Int -> Bool
allSpansHaveWidth ops expected
    = all (== expected) $ Vector.toList $ Vector.map spanOpsEffectedColumns ops

spanOpsEffectedRows :: DisplayOps -> Int
spanOpsEffectedRows ops
    = toEnum $ length (filter (not . null . Vector.toList) (Vector.toList ops))

type SpanConstructLog = [SpanConstructEvent]
data SpanConstructEvent = SpanSetAttr Attr

isSetAttr :: Attr -> SpanConstructEvent -> Bool
isSetAttr expectedAttr (SpanSetAttr inAttr)
    | inAttr == expectedAttr = True
isSetAttr _attr _event = False

data MockWindow = MockWindow Int Int
    deriving (Show, Eq)

regionForWindow :: MockWindow -> DisplayRegion
regionForWindow (MockWindow w h) = (w,h)
