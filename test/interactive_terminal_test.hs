{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Graphics.Vty.Attributes
import Graphics.Vty.AttributeChange
import Graphics.Vty.Image
import Graphics.Vty.Picture
import Graphics.Vty.Terminal
import Graphics.Vty.WinRegion

import Control.Monad

import Data.List ( lookup )
import Data.Maybe ( isJust, fromJust )
import Data.Monoid
import Data.Word

import Foreign.Marshal.Array 
import HereDoc

import qualified System.Environment as Env

import System.IO ( hFlush, hPutStr, hPutBuf, stdout )

main = do
    print_intro

output_file_path = "test_results.list"

print_intro = do
    putStr $ [$heredoc| 
This is an interactive verification program for the terminal input and output
support of the VTY library. This will ask a series of questions about what you
see onscreen. The goal is to verify that VTY's output   and input support
performs as expected with your terminal.

This program produces a file named 
    |] ++ output_file_path ++ [$heredoc| 
in the current directory that contains the results for each test assertion. This
can  be emailed to coreyoconnor@gmail.com and used by the VTY authors to improve
support for your terminal. No personal information is contained in the report.

Each test follows, more or less, the following format:
    0. A description of the test is printed which will include a detailed
    description of what VTY is going to try and what the   expected results are.
    Press return to move on.
    1. The program will produce some output or ask for you to press a key.
    2. You will then be asked to confirm if the behavior matched the provided
    description.  Just pressing enter implies the default response that
    everything was as expected. 

All the tests assume the following about the terminal display:
    0. The terminal display will not be resized during a test and is at least 80 
    characters in width. 
    1. The terminal display is using a monospaced font for both single width and
    double width characters.
    2. A double width character is displayed with exactly twice the width of a 
    single column character. This may require adjusting the font used by the
    terminal. At least, that is the case using xterm. 
    3. Fonts are installed, and usable by the terminal, that define glyphs for
    a good range of the unicode characters. Each test involving unicode display
    describes the expected appearance of each glyph. 

Thanks for the help! :-D
|]
    wait_for_return
    results <- do_test_menu 1
    env_attributes <- mapM ( \env_name -> catch ( Env.getEnv env_name >>= return . (,) env_name ) 
                                                ( const $ return (env_name, "") ) 
                           ) 
                           [ "TERM", "COLORTERM", "LANG" ]
    let results_txt = show env_attributes ++ "\n" ++ show results ++ "\n"
    writeFile output_file_path results_txt

wait_for_return = do
    putStr "\n(press return to continue)"
    hFlush stdout
    getLine

test_menu :: [(String, Test)]
test_menu = zip (map show [1..]) all_tests

do_test_menu :: Int -> IO [(String, Bool)]
do_test_menu next_ID 
    | next_ID > length all_tests = do
        putStrLn $ "Done! Please email the " ++ output_file_path ++ " file to coreyoconnor@gmail.com"
        return []
    | otherwise = do
        display_test_menu
        putStrLn $ "Press return to start with #" ++ show next_ID ++ "."
        putStrLn "Enter a test number to perform only that test."
        putStrLn "q (or control-C) to quit."
        putStr "> "
        hFlush stdout
        s <- getLine >>= return . filter (/= '\n')
        case s of
            "q" -> return mempty
            "" -> do 
                r <- run_test $ show next_ID 
                rs <- do_test_menu ( next_ID + 1 )
                return $ r : rs
            i | isJust ( lookup i test_menu ) -> do
                r <- run_test i 
                rs <- do_test_menu ( read i + 1 )
                return $ r : rs
        where
            display_test_menu 
                = mapM_ display_test_menu' test_menu
            display_test_menu' ( i, t ) 
                = putStrLn $ ( if i == show next_ID 
                                then "> " 
                                else "  "
                             ) ++ i ++ ". " ++ test_name t

run_test :: String -> IO (String, Bool)
run_test i = do
    let t = fromJust $ lookup i test_menu 
    print_summary t
    wait_for_return
    test_action t
    r <- confirm_results t
    return (test_ID t, r)

default_success_confirm_results = do
    putStr "\n"
    putStr "[Y/n] "
    hFlush stdout
    r <- getLine
    case r of
        "" -> return True
        "y" -> return True
        "Y" -> return True
        "n" -> return False

data Test = Test
    { test_name :: String
    , test_ID :: String
    , test_action :: IO ()
    , print_summary :: IO ()
    , confirm_results :: IO Bool
    }

all_tests 
    = [ reserve_output_test 
      , display_bounds_test_0
      , display_bounds_test_1
      , display_bounds_test_2
      , display_bounds_test_3
      , unicode_single_width_0
      , unicode_single_width_1
      , unicode_double_width_0
      , unicode_double_width_1
      , attributes_test_0
      , attributes_test_1
      , attributes_test_2
      , attributes_test_3
      , attributes_test_4
      , attributes_test_5
      , inline_test_0
      ]

reserve_output_test = Test 
    { test_name = "Initialize and reserve terminal output then restore previous state."
    , test_ID = "reserve_output_test"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        putStrLn "Line 1"
        putStrLn "Line 2"
        putStrLn "Line 3"
        putStrLn "Line 4 (press return)"
        hFlush stdout
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = do
        putStr $ [$heredoc|
Once return is pressed:
The screen will be cleared. 
The cursor should be visible and at the top left corner.
Four lines of text should be visible.

After enter is pressed for the second time this test then:
    * The screen containing the test summary should be restored;
    * The cursor is visible.
|]
    , confirm_results = do
        putStr $ [$heredoc|
Did the test output match the description?
|]
        default_success_confirm_results
    }

display_bounds_test_0 = Test
    { test_name = "Verify display bounds are correct test 0: Using spaces."
    , test_ID = "display_bounds_test_0"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        DisplayBounds w h <- display_bounds t
        let row_0 = replicate (fromEnum w) 'X' ++ "\n"
            row_h = replicate (fromEnum w - 1) 'X'
            row_n = "X" ++ replicate (fromEnum w - 2) ' ' ++ "X\n"
            image = row_0 ++ (concat $ replicate (fromEnum h - 2) row_n) ++ row_h
        putStr image
        hFlush stdout
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = display_bounds_test_summary True
    , confirm_results = generic_output_match_confirm
    }

display_bounds_test_1 = Test
    { test_name = "Verify display bounds are correct test 0: Using cursor movement."
    , test_ID = "display_bounds_test_1"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        DisplayBounds w h <- display_bounds t
        set_cursor_pos t 0 0
        let row_0 = replicate (fromEnum w) 'X' ++ "\n"
        putStr row_0
        forM_ [1 .. h - 2] $ \y -> do
            set_cursor_pos t 0 y
            putStr "X"
            set_cursor_pos t (w - 1) y
            putStr "X"
        set_cursor_pos t 0 (h - 1)
        let row_h = replicate (fromEnum w - 1) 'X'
        putStr row_h
        hFlush stdout
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = display_bounds_test_summary True
    , confirm_results = generic_output_match_confirm
    }

display_bounds_test_2 = Test
    { test_name = "Verify display bounds are correct test 0: Using Image ops."
    , test_ID = "display_bounds_test_2"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        bounds@(DisplayBounds w h) <- display_bounds t
        let first_row = horiz_cat $ replicate (fromEnum w) (char def_attr 'X')
            middle_rows = vert_cat $ replicate (fromEnum h - 2) middle_row
            middle_row = (char def_attr 'X') <|> background_fill (w - 2) 1 <|> (char def_attr 'X')
            end_row = first_row
            image = first_row <-> middle_rows <-> end_row
            pic = (pic_for_image image) { pic_cursor = Cursor (w - 1) (h - 1) }
        d <- display_context t bounds
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = display_bounds_test_summary True
    , confirm_results = generic_output_match_confirm
    }

display_bounds_test_3 = Test
    { test_name = "Verify display bounds are correct test 0: Hide cursor; Set cursor pos."
    , test_ID = "display_bounds_test_3"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        DisplayBounds w h <- display_bounds t
        hide_cursor t
        set_cursor_pos t 0 0
        let row_0 = replicate (fromEnum w) 'X'
        putStrLn row_0
        forM_ [1 .. h - 2] $ \y -> do
            set_cursor_pos t 0 y
            putStr "X"
            set_cursor_pos t (w - 1) y
            putStr "X"
        set_cursor_pos t 0 (h - 1)
        let row_h = row_0
        putStr row_h
        hFlush stdout
        getLine
        show_cursor t
        release_display t
        release_terminal t
        return ()
    , print_summary = display_bounds_test_summary False
    , confirm_results = generic_output_match_confirm
    }

display_bounds_test_summary has_cursor = do
    putStr $ [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
|]
    if has_cursor
        then putStr "    1. The cursor will be visible."
        else putStr "    1. The cursor will NOT be visible."
    putStr [$heredoc|
    2. The border of the display will be outlined in Xs. 
       So if - and | represented the edge of the terminal window:
         |-------------|
         |XXXXXXXXXXXXX|
         |X           X||]

    if has_cursor
        then putStr $ [$heredoc|
         |XXXXXXXXXXXXC| |]
        else putStr $ [$heredoc|
         |XXXXXXXXXXXXX| |]

    putStr $ [$heredoc|
         |-------------|

        ( Where C is the final position of the cursor. There may be an X drawn
        under the cursor. )
    3. The display will remain in this state until return is pressed again.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]

generic_output_match_confirm = do
    putStr $ [$heredoc|
Did the test output match the description?
|]
    default_success_confirm_results

-- Explicitely definethe bytes that encode each example text.
-- This avoids any issues with how the compiler represents string literals.
--
-- This document is UTF-8 encoded so the UTF-8 string is still included for
-- reference
--
-- It's assumed the compiler will at least not barf on UTF-8 encoded text in
-- comments ;-)
--
-- txt_0 = ↑↑↓↓←→←→BA

utf8_txt_0 :: [[Word8]]
utf8_txt_0 = [ [ 0xe2 , 0x86 , 0x91 ]
             , [ 0xe2 , 0x86 , 0x91 ]
             , [ 0xe2 , 0x86 , 0x93 ]
             , [ 0xe2 , 0x86 , 0x93 ]
             , [ 0xe2 , 0x86 , 0x90 ]
             , [ 0xe2 , 0x86 , 0x92 ]
             , [ 0xe2 , 0x86 , 0x90 ]
             , [ 0xe2 , 0x86 , 0x92 ]
             , [ 0x42 ]
             , [ 0x41 ]
             ]

iso_10646_txt_0 :: String
iso_10646_txt_0 = map toEnum
    [ 8593 
    , 8593
    , 8595
    , 8595
    , 8592
    , 8594
    , 8592
    , 8594
    , 66
    , 65
    ]

unicode_single_width_0 = Test
    { test_name = "Verify terminal can display unicode single-width characters. (Direct UTF-8)"
    , test_ID = "unicode_single_width_0"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        withArrayLen (concat utf8_txt_0) (flip $ hPutBuf stdout)
        hPutStr stdout "\n"
        hPutStr stdout "0123456789\n"
        hFlush stdout
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = unicode_single_width_summary
    , confirm_results = generic_output_match_confirm
    }

unicode_single_width_1 = Test
    { test_name = "Verify terminal can display unicode single-width characters. (Image ops)"
    , test_ID = "unicode_single_width_1"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = line_0 <-> line_1
            line_0 = iso_10646_string def_attr iso_10646_txt_0
            line_1 = string def_attr "0123456789"
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = unicode_single_width_summary
    , confirm_results = generic_output_match_confirm
    }

unicode_single_width_summary = putStr [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. Two horizontal lines of text will be displayed:
        a. The first will be a sequence of glyphs in UTF-8 encoding. Each glyph
        will occupy one column of space. The order and appearance of the glyphs
        will be:
            | column | appearance    |
            ==========================
            | 0      | up arrow      |
            | 1      | up arrow      |
            | 2      | down arrow    |
            | 3      | down arrow    |
            | 4      | left arrow    |
            | 5      | right arrow   |
            | 6      | left arrow    |
            | 7      | right arrow   |
            | 8      | B             |
            | 9      | A             |
            ( see: http://en.wikipedia.org/wiki/Arrow_(symbol) )
        b. The second will be: 0123456789. 

Verify: 
    * The far right extent of the glyphs on both lines are equal; 
    * The glyphs are as described.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]

-- The second example is a unicode string containing double-width glyphs
-- 你好吗
utf8_txt_1 :: [[Word8]]
utf8_txt_1 = [ [0xe4,0xbd,0xa0]
             , [0xe5,0xa5,0xbd]
             , [0xe5,0x90,0x97]
             ]

iso_10646_txt_1 :: String
iso_10646_txt_1 = map toEnum [20320,22909,21527]

unicode_double_width_0 = Test
    { test_name = "Verify terminal can display unicode double-width characters. (Direct UTF-8)"
    , test_ID = "unicode_double_width_0"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        withArrayLen (concat utf8_txt_1) (flip $ hPutBuf stdout)
        hPutStr stdout "\n"
        hPutStr stdout "012345\n"
        hFlush stdout
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = unicode_double_width_summary
    , confirm_results = generic_output_match_confirm
    }

unicode_double_width_1 = Test
    { test_name = "Verify terminal can display unicode double-width characters. (Image ops)"
    , test_ID = "unicode_double_width_1"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = line_0 <-> line_1
            line_0 = iso_10646_string def_attr iso_10646_txt_1
            line_1 = string def_attr "012345"
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = unicode_double_width_summary
    , confirm_results = generic_output_match_confirm
    }

unicode_double_width_summary = putStr [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. Two horizontal lines of text will be displayed:
        a. The first will be a sequence of glyphs in UTF-8 encoding. Each glyph
        will occupy two columns of space. The order and appearance of the glyphs
        will be:
            | column | appearance                |
            ======================================
            | 0      | first half of ni3         |
            | 1      | second half of ni3        |
            | 2      | first half of hao3        |
            | 3      | second half of hao3       |
            | 4      | first half of ma          |
            | 5      | second half of ma         |
        b. The second will be: 012345. 

Verify: 
    * The far right extent of the glyphs on both lines are equal; 
    * The glyphs are as described.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]

all_colors = zip [ black, red, green, yellow, blue, magenta, cyan, white, def ]
                 [ "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white", "default" ]

attributes_test_0 = Test 
    { test_name = "Character attributes: foreground colors."
    , test_ID = "attributes_test_0"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = border <|> column_0 <|> border <|> column_1 <|> border
            column_0 = vert_cat $ map line_with_color all_colors
            border = vert_cat $ replicate (length all_colors) $ string def_attr " | "
            column_1 = vert_cat $ map (string def_attr . snd) all_colors
            line_with_color (c_value, c_name) = string (setFG c_value def_attr) c_name
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = do
        putStr $ [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. 9 lines of text in two columns will be drawn. The first column will be a
    name of a standard color (for an 8 color terminal) rendered in that color.
    For instance, one line will be the word "magenta" and that word should be
    rendered in the magenta color. The second column will be the name of a
    standard color rendered with the default attributes.

Verify: 
    * In the first column: The foreground color matches the named color.
    * The second column: All text is rendered with the default attributes.
    * The vertical bars used in each line to mark the border of a column are
    lined up.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]
    , confirm_results = do
        putStr $ [$heredoc|
Did the test output match the description?
|]
        default_success_confirm_results
    }

attributes_test_1 = Test 
    { test_name = "Character attributes: background colors."
    , test_ID = "attributes_test_1"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = border <|> column_0 <|> border <|> column_1 <|> border
            column_0 = vert_cat $ map line_with_color all_colors
            border = vert_cat $ replicate (length all_colors) $ string def_attr " | "
            column_1 = vert_cat $ map (string def_attr . snd) all_colors
            line_with_color (c_value, c_name) = string (setBG c_value def_attr) c_name
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = do
        putStr $ [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. 9 lines of text in two columns will be drawn. The first column will
    contain be a name of a standard color for an 8 color terminal rendered with
    the default foreground color with a background the named color.  For
    instance, one line will contain be the word "magenta" and that words should
    be rendered in the default foreground color over a magenta background. The
    second column will be the name of a standard color rendered with the default
    attributes.

Verify: 
    * The first column: The background color matches the named color.
    * The second column: All text is rendered with the default attributes.
    * The vertical bars used in each line to mark the border of a column are
    lined up.

Note: I haven't decided if, in this case, the background color should extend to
fills added for alignment. Right now the selected background color is only
applied to the background where the word is actually rendered. Since each word
is not of the same length VTY adds background fills to make the width of each
row effectively the same. These added fills are all currently rendered with the
default background pattern.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]
    , confirm_results = do
        putStr $ [$heredoc|
Did the test output match the description?
|]
        default_success_confirm_results
    }

attributes_test_2 = Test 
    { test_name = "Character attributes: Vivid foreground colors."
    , test_ID = "attributes_test_2"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = horiz_cat [border, column_0, border, column_1, border, column_2, border]
            border = vert_cat $ replicate (length all_colors) $ string def_attr " | "
            column_0 = vert_cat $ map line_with_color_0 all_colors
            column_1 = vert_cat $ map line_with_color_1 all_colors
            column_2 = vert_cat $ map (string def_attr . snd) all_colors
            line_with_color_0 (c_value, c_name) = string (setFG c_value def_attr) c_name
            line_with_color_1 (c_value, c_name) = string (setFGVivid c_value def_attr) c_name
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = do
        putStr $ [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. 9 lines of text in three columns will be drawn:
        a. The first column will be a name of a standard color (for an 8 color
        terminal) rendered with that color as the foreground color.  
        b. The next column will be also be the name of a standard color rendered
        with that color as the foreground color but the shade used should be
        more vivid than the shade used in the first column.    
        c. The final column will be the name of a color rendered with the
        default attributes.

For instance, one line will be the word "magenta" and that word should be
rendered in the magenta color. 

I'm not actually sure exactly what "vivid" means in this context. For xterm the
vivid colors are brighter.  

Verify: 
    * The first column: The foreground color matches the named color.
    * The second column: The foreground color matches the named color but is
    more vivid than the color used in the first column.  
    * The third column: All text is rendered with the default attributes.
    * The vertical bars used in each line to mark the border of a column are
    lined up.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]
    , confirm_results = do
        putStr $ [$heredoc|
Did the test output match the description?
|]
        default_success_confirm_results
    }

attributes_test_3 = Test 
    { test_name = "Character attributes: vivid background colors."
    , test_ID = "attributes_test_3"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = horiz_cat [border, column_0, border, column_1, border, column_2, border]
            border = vert_cat $ replicate (length all_colors) $ string def_attr " | "
            column_0 = vert_cat $ map line_with_color_0 all_colors
            column_1 = vert_cat $ map line_with_color_1 all_colors
            column_2 = vert_cat $ map (string def_attr . snd) all_colors
            line_with_color_0 (c_value, c_name) = string (setBG c_value def_attr) c_name
            line_with_color_1 (c_value, c_name) = string (setBGVivid c_value def_attr) c_name
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = do
        putStr $ [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. 9 lines of text in three columns will be drawn:
        a. The first column will contain be a name of a standard color for an 8
        color terminal rendered with the default foreground color with a
        background the named color.  
        b. The first column will contain be a name of a standard color for an 8
        color terminal rendered with the default foreground color with the
        background a vivid version of the named color. 
        c. The third column will be the name of a standard color rendered with
        the default attributes.
        
For instance, one line will contain be the word "magenta" and that words should
be rendered in the default foreground color over a magenta background. 

I'm not actually sure exactly what "vivid" means in this context. For xterm the
vivid colors are brighter.

Verify: 
    * The first column: The background color matches the named color.
    * The second column: The background color matches the named color and is
    more vivid than the color used in the first column.  
    * The third column column: All text is rendered with the default attributes.
    * The vertical bars used in each line to mark the border of a column are
    lined up.

Note: I haven't decided if, in this case, the background color should extend to
fills added for alignment. Right now the selected background color is only
applied to the background where the word is actually rendered. Since each word
is not of the same length VTY adds background fills to make the width of each
row effectively the same. These added fills are all currently rendered with the
default background pattern.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]
    , confirm_results = do
        putStr $ [$heredoc|
Did the test output match the description?
|]
        default_success_confirm_results
    }

attr_combos = 
    [ ("default", id)
    , ("bold", setBold)
    , ("blink", setBlink)
    , ("underline", setUnderline)
    , ("bold + blink", setBlink . setBold)
    , ("bold + underline", setUnderline . setBold)
    , ("underline + blink", setBlink . setUnderline)
    , ("bold + blink + underline", setUnderline . setBlink . setBold)
    ]

attributes_test_4 = Test 
    { test_name = "Character attributes: Bold; Blink; Underline."
    , test_ID = "attributes_test_4"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = horiz_cat [border, column_0, border, column_1, border]
            border = vert_cat $ replicate (length attr_combos) $ string def_attr " | "
            column_0 = vert_cat $ map line_with_attrs attr_combos
            column_1 = vert_cat $ map (string def_attr . fst) attr_combos
            line_with_attrs (desc, attr_f) = string (attr_f def_attr) desc
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = do
        putStr $ [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. 8 rows of text in two columns. 
    The rows will contain the following text:
        default
        bold 
        blink
        underline
        bold + blink
        bold + underline
        underline + blink
        bold + blink + underline
    The first column will be rendered with the described attributes. The second
    column will be rendered with the default attributes.
        
Verify: 
    * The vertical bars used in each line to mark the border of a column are
    lined up.
    * The text in the first column is rendered as described.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]
    , confirm_results = do
        putStr $ [$heredoc|
Did the test output match the description?
|]
        default_success_confirm_results
    }

attributes_test_5 = Test 
    { test_name = "Character attributes: 240 color palette"
    , test_ID = "attributes_test_5"
    , test_action = do
        t <- terminal_handle
        reserve_display t
        let pic = pic_for_image image
            image = vert_cat $ map horiz_cat $ split_color_images color_images
            color_images = map (\i -> string (current_attr `with_back_color` Color240 i) " ") [0..239]
            split_color_images [] = []
            split_color_images is = (take 20 is ++ [string def_attr " "]) : (split_color_images (drop 20 is))
        d <- display_bounds t >>= display_context t
        output_picture d pic
        getLine
        release_display t
        release_terminal t
        return ()
    , print_summary = do
        putStr $ [$heredoc|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. A 20 character wide and 12 row high block of color squares. This should look like a palette
    of some sort. I'm not exactly sure if all color terminals use the same palette. I doubt it...

Verify: 

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]
    , confirm_results = do
        putStr $ [$heredoc|
Did the test output match the description?
|]
        default_success_confirm_results
    }

inline_test_0 = Test
    { test_name = "Verify styled output can be performed without clearing the screen."
    , test_ID = "inline_test_0"
    , test_action = do
        t <- terminal_handle
        putStrLn "line 0."
        put_attr_change t $ back_color red >> style underline
        putStrLn "line 1."
        put_attr_change t $ default_all
        putStrLn "line 2."
        release_terminal t
        return ()
    , print_summary = putStr $ [$heredoc|
lines are in order.
the second line "line 1" should have a red background and the text underline.
|]

    , confirm_results = generic_output_match_confirm
    }
