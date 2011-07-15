{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verify.Graphics.Vty.DisplayRegion
import Verify.Graphics.Vty.Picture
import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Span
import Graphics.Vty.Terminal
import Graphics.Vty.Terminal.Debug

import Graphics.Vty.Debug

import Verify

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

import System.IO

unit_image_unit_bounds :: UnitImage -> Property
unit_image_unit_bounds (UnitImage _ i) = liftIOResult $ do
    t <- terminal_instance (DisplayRegion 1 1)
    d <- display_bounds t >>= display_context t
    let pic = pic_for_image i
    output_picture d pic
    return succeeded

unit_image_arb_bounds :: UnitImage -> DebugWindow -> Property
unit_image_arb_bounds (UnitImage _ i) (DebugWindow w h) = liftIOResult $ do
    t <- terminal_instance (DisplayRegion w h)
    d <- display_bounds t >>= display_context t
    let pic = pic_for_image i
    output_picture d pic
    return succeeded

single_T_row :: DebugWindow -> Property
single_T_row (DebugWindow w h) = liftIOResult $ do
    t <- terminal_instance (DisplayRegion w h)
    d <- display_bounds t >>= display_context t
    -- create an image that contains just the character T repeated for a single row
    let i = horiz_cat $ replicate (fromEnum w) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture d pic
    out_bytes <- readIORef (debug_terminal_last_output $ dehandle t) >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes a single line containing the T string:
    let expected = "HD" ++ "MA" ++ replicate (fromEnum w) 'T'
    -- Followed by h - 1 lines of a change to the background attribute and then the background
    -- character
                   ++ concat (replicate (fromEnum h - 1) $ "MA" ++ replicate (fromEnum w) 'B')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    if out_bytes /=  expected_bytes
        then return $ failed { reason = "\n" ++ show out_bytes ++ "\n\n" ++ show expected_bytes }
        else return succeeded
    
many_T_rows :: DebugWindow -> Property
many_T_rows (DebugWindow w h) = liftIOResult $ do
    t <- terminal_instance (DisplayRegion w h)
    d <- display_bounds t >>= display_context t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vert_cat $ replicate (fromEnum h) $ horiz_cat $ replicate (fromEnum w) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture d pic
    out_bytes <- readIORef (debug_terminal_last_output $ dehandle t) >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w 'T's
    let expected = "HD" ++ concat (replicate (fromEnum h) $ "MA" ++ replicate (fromEnum w) 'T')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    if out_bytes /=  expected_bytes
        then return $ failed { reason = "\n" ++ show out_bytes ++ "\n\n" ++ show expected_bytes }
        else return succeeded

many_T_rows_cropped_width :: DebugWindow -> Property
many_T_rows_cropped_width (DebugWindow w h) = liftIOResult $ do
    t <- terminal_instance (DisplayRegion w h)
    d <- display_bounds t >>= display_context t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vert_cat $ replicate (fromEnum h) $ horiz_cat $ replicate (fromEnum w * 2) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture d pic
    out_bytes <- readIORef (debug_terminal_last_output $ dehandle t) >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w 'T's
    let expected = "HD" ++ concat (replicate (fromEnum h) $ "MA" ++ replicate (fromEnum w) 'T')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    if out_bytes /=  expected_bytes
        then return $ failed { reason = "\n" ++ show out_bytes ++ "\n\n" ++ show expected_bytes }
        else return succeeded

many_T_rows_cropped_height :: DebugWindow -> Property
many_T_rows_cropped_height (DebugWindow w h) = liftIOResult $ do
    t <- terminal_instance (DisplayRegion w h)
    d <- display_bounds t >>= display_context t
    -- create an image that contains the character 'T' repeated for all the rows
    let i = vert_cat $ replicate (fromEnum h * 2) $ horiz_cat $ replicate (fromEnum w) (char def_attr 'T')
        pic = (pic_for_image i) { pic_background = Background 'B' def_attr }
    output_picture d pic
    out_bytes <- readIORef (debug_terminal_last_output $ dehandle t) >>= return . UTF8.toRep
    -- The UTF8 string that represents the output bytes is h repeats of a move, 'M', followed by an
    -- attribute change. 'A', followed by w count 'T's
    let expected = "HD" ++ concat (replicate (fromEnum h) $ "MA" ++ replicate (fromEnum w) 'T')
        expected_bytes :: BS.ByteString = UTF8.toRep $ UTF8.fromString expected
    if out_bytes /=  expected_bytes
        then return $ failed { reason = "\n" ++ show out_bytes ++ "\n\n" ++ show expected_bytes }
        else return succeeded

main = run_test $ do
    verify "unit_image_unit_bounds" unit_image_unit_bounds
    verify "unit_image_arb_bounds" unit_image_arb_bounds
    verify "single_T_row" single_T_row
    verify "many_T_rows" many_T_rows
    verify "many_T_rows_cropped_width" many_T_rows_cropped_width
    verify "many_T_rows_cropped_height" many_T_rows_cropped_height
    return ()

