module VerifyUsingMockTerminal where

import Verify.Graphics.Vty.Prelude

import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Span
import Verify.Graphics.Vty.Output
import Graphics.Vty.Output
import Graphics.Vty.Output.Mock

import Graphics.Vty.Debug

import Verify

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

import System.IO

unitImageUnitBounds :: UnitImage -> Property
unitImageUnitBounds (UnitImage _ i) = liftIOResult $ do
    (_,t) <- mockTerminal (1,1)
    dc <- displayBounds t >>= displayContext t
    let pic = picForImage i
    outputPicture dc pic
    return succeeded

unitImageArbBounds :: UnitImage -> MockWindow -> Property
unitImageArbBounds (UnitImage _ i) (MockWindow w h) = liftIOResult $ do
    (_,t) <- mockTerminal (w,h)
    dc <- displayBounds t >>= displayContext t
    let pic = picForImage i
    outputPicture dc pic
    return succeeded

singleTRow :: MockWindow -> Property
singleTRow (MockWindow w h) = liftIOResult $ do
    (mockData,t) <- mockTerminal (w,h)
    dc <- displayBounds t >>= displayContext t
    -- create an image that contains just the character T repeated for a single row
    let i = horizCat $ replicate (fromEnum w) (char defAttr 'T')
        pic = (picForImage i) { picBackground = Background 'B' defAttr }
    outputPicture dc pic
    -- The mock output string that represents the output bytes a single line containing the T
    -- string: Followed by h - 1 lines of a change to the background attribute and then the
    -- background character
    let expected = "H" ++ "MDA" ++ replicate (fromEnum w) 'T'
                 ++ concat (replicate (fromEnum h - 1) $ "MDA" ++ replicate (fromEnum w) 'B')
    compareMockOutput mockData expected
    
manyTRows :: MockWindow -> Property
manyTRows (MockWindow w h) = liftIOResult $ do
    (mockData, t) <- mockTerminal (w,h)
    dc <- displayBounds t >>= displayContext t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vertCat $ replicate (fromEnum h) $ horizCat $ replicate (fromEnum w) (char defAttr 'T')
        pic = (picForImage i) { picBackground = Background 'B' defAttr }
    outputPicture dc pic
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w 'T's
    let expected = "H" ++ concat (replicate (fromEnum h) $ "MDA" ++ replicate (fromEnum w) 'T')
    compareMockOutput mockData expected

manyTRowsCroppedWidth :: MockWindow -> Property
manyTRowsCroppedWidth (MockWindow w h) = liftIOResult $ do
    (mockData,t) <- mockTerminal (w,h)
    dc <- displayBounds t >>= displayContext t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vertCat $ replicate (fromEnum h) $ horizCat $ replicate (fromEnum w * 2) (char defAttr 'T')
        pic = (picForImage i) { picBackground = Background 'B' defAttr }
    outputPicture dc pic
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w 'T's
    let expected = "H" ++ concat (replicate (fromEnum h) $ "MDA" ++ replicate (fromEnum w) 'T')
    compareMockOutput mockData expected

manyTRowsCroppedHeight :: MockWindow -> Property
manyTRowsCroppedHeight (MockWindow w h) = liftIOResult $ do
    (mockData,t) <- mockTerminal (w,h)
    dc <- displayBounds t >>= displayContext t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vertCat $ replicate (fromEnum h * 2) $ horizCat $ replicate (fromEnum w) (char defAttr 'T')
        pic = (picForImage i) { picBackground = Background 'B' defAttr }
    outputPicture dc pic
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w count 'T's
    let expected = "H" ++ concat (replicate (fromEnum h) $ "MDA" ++ replicate (fromEnum w) 'T')
    compareMockOutput mockData expected

tests :: IO [Test]
tests = return [ verify "unitImageUnitBounds" unitImageUnitBounds
               , verify "unitImageArbBounds" unitImageArbBounds
               , verify "singleTRow" singleTRow
               , verify "manyTRows" manyTRows
               , verify "manyTRowsCroppedWidth" manyTRowsCroppedWidth
               , verify "manyTRowsCroppedHeight" manyTRowsCroppedHeight
               ]

