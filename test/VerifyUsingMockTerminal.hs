{-# LANGUAGE ScopedTypeVariables #-}
module VerifyUsingMockTerminal where

import Verify.Graphics.Vty.DisplayRegion
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Span
import Graphics.Vty.Terminal
import Graphics.Vty.Terminal.Mock

import Graphics.Vty.Debug

import Verify

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

import System.IO

compare_bytes out_bytes expected_bytes =
    if out_bytes /=  expected_bytes
        then return $ failed { reason = "bytes\n" ++ show out_bytes
                                        ++ "\nare not the expected bytes\n"
                                        ++ show expected_bytes
                             }
        else return succeeded

unit_image_unit_bounds :: UnitImage -> Property
unit_image_unit_bounds (UnitImage _ i) = liftIOResult $ do
    (_,t) <- mock_terminal (DisplayRegion 1 1)
    dc <- display_bounds t >>= display_context t
    let pic = pic_for_image i
    output_picture dc pic
    return succeeded

unit_image_arb_bounds :: UnitImage -> MockWindow -> Property
unit_image_arb_bounds (UnitImage _ i) (MockWindow w h) = liftIOResult $ do
    (_,t) <- mock_terminal (DisplayRegion w h)
    dc <- display_bounds t >>= display_context t
    let pic = pic_for_image i
    output_picture dc pic
    return succeeded

single_T_row :: MockWindow -> Property
single_T_row (MockWindow w h) = liftIOResult $ do
    (mock_data,t) <- mock_terminal (DisplayRegion w h)
    dc <- display_bounds t >>= display_context t
    -- create an image that contains just the character T repeated for a single row
    let i = horiz_cat $ replicate (fromEnum w) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture dc pic
    out_bytes <- readIORef mock_data >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes a single line containing the T string:
    let expected = "HD" ++ "MA" ++ replicate (fromEnum w) 'T'
    -- Followed by h - 1 lines of a change to the background attribute and then the background
    -- character
                   ++ concat (replicate (fromEnum h - 1) $ "MA" ++ replicate (fromEnum w) 'B')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    compare_bytes out_bytes expected_bytes
    
many_T_rows :: MockWindow -> Property
many_T_rows (MockWindow w h) = liftIOResult $ do
    (mock_data, t) <- mock_terminal (DisplayRegion w h)
    dc <- display_bounds t >>= display_context t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vert_cat $ replicate (fromEnum h) $ horiz_cat $ replicate (fromEnum w) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture dc pic
    out_bytes <- readIORef mock_data >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w 'T's
    let expected = "HD" ++ concat (replicate (fromEnum h) $ "MA" ++ replicate (fromEnum w) 'T')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    compare_bytes out_bytes expected_bytes

many_T_rows_cropped_width :: MockWindow -> Property
many_T_rows_cropped_width (MockWindow w h) = liftIOResult $ do
    (mock_data,t) <- mock_terminal (DisplayRegion w h)
    dc <- display_bounds t >>= display_context t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vert_cat $ replicate (fromEnum h) $ horiz_cat $ replicate (fromEnum w * 2) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture dc pic
    out_bytes <- readIORef mock_data >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w 'T's
    let expected = "HD" ++ concat (replicate (fromEnum h) $ "MA" ++ replicate (fromEnum w) 'T')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    compare_bytes out_bytes expected_bytes

many_T_rows_cropped_height :: MockWindow -> Property
many_T_rows_cropped_height (MockWindow w h) = liftIOResult $ do
    (mock_data,t) <- mock_terminal (DisplayRegion w h)
    dc <- display_bounds t >>= display_context t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vert_cat $ replicate (fromEnum h * 2) $ horiz_cat $ replicate (fromEnum w) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture dc pic
    out_bytes <- readIORef mock_data >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w count 'T's
    let expected = "HD" ++ concat (replicate (fromEnum h) $ "MA" ++ replicate (fromEnum w) 'T')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    compare_bytes out_bytes expected_bytes

tests :: IO [Test]
tests = return [ verify "unit_image_unit_bounds" unit_image_unit_bounds
               , verify "unit_image_arb_bounds" unit_image_arb_bounds
               , verify "single_T_row" single_T_row
               , verify "many_T_rows" many_T_rows
               , verify "many_T_rows_cropped_width" many_T_rows_cropped_width
               , verify "many_T_rows_cropped_height" many_T_rows_cropped_height
               ]

