{-# LANGUAGE QuasiQuotes #-}
module Main where

import Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Inline
import Graphics.Vty.Picture
import Graphics.Vty.Output

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad

import Data.Default (def)
import Data.List ( lookup )
import Data.Maybe ( isJust, fromJust )
import Data.Monoid
import Data.String.QQ
import Data.Word

import Foreign.Marshal.Array 

import qualified System.Environment as Env

import System.IO ( hFlush, hPutStr, hPutBuf, stdout )

main = do
    printIntro

outputFilePath = "test_results.list"

printIntro = do
    putStr $ [s| 
This is an interactive verification program for the terminal input and output
support of the VTY library. This will ask a series of questions about what you
see on screen. The goal is to verify that VTY's output and input support
performs as expected with your terminal.

This program produces a file named 
    |] ++ outputFilePath ++ [s| 
in the current directory that contains the results for each test assertion. This
can  be emailed to coreyoconnor@gmail.com and used by the VTY authors to improve
support for your terminal. No personal information is contained in the report.

Each test follows, more or less, the following format:
    0. A description of the test is printed which will include a detailed
    description of what VTY is going to try and what the expected results are.
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
To exit the test early enter "q" anytime at the following menu screen.

If any test failed then please post an issue to
    https://github.com/coreyoconnor/vty/issues
with the test_results.list file pasted into the issue. The issue summary can
mention the specific tests that failed or just say "interactive terminal test
failure".
|]
    waitForReturn
    results <- doTestMenu 1
    envAttributes <- mapM ( \envName -> Control.Exception.catch ( Env.getEnv envName >>= return . (,) envName ) 
                                                ( \ (_ :: SomeException) -> return (envName, "") ) 
                          ) 
                          [ "TERM", "COLORTERM", "LANG", "TERM_PROGRAM", "XTERM_VERSION" ]
    t <- outputForCurrentTerminal
    let resultsTxt = show envAttributes ++ "\n" 
                     ++ terminalID t ++ "\n"
                     ++ show results ++ "\n"
    releaseTerminal t
    writeFile outputFilePath resultsTxt

waitForReturn = do
    putStr "\n(press return to continue)"
    hFlush stdout
    getLine

testMenu :: [(String, Test)]
testMenu = zip (map show [1..]) allTests

doTestMenu :: Int -> IO [(String, Bool)]
doTestMenu nextID 
    | nextID > length allTests = do
        putStrLn $ "Done! Please email the " ++ outputFilePath ++ " file to coreyoconnor@gmail.com"
        return []
    | otherwise = do
        displayTestMenu
        putStrLn $ "Press return to start with #" ++ show nextID ++ "."
        putStrLn "Enter a test number to perform only that test."
        putStrLn "q (or control-C) to quit."
        putStr "> "
        hFlush stdout
        s <- getLine >>= return . filter (/= '\n')
        case s of
            "q" -> return mempty
            "" -> do 
                r <- runTest $ show nextID 
                rs <- doTestMenu ( nextID + 1 )
                return $ r : rs
            i | isJust ( lookup i testMenu ) -> do
                r <- runTest i 
                rs <- doTestMenu ( read i + 1 )
                return $ r : rs
        where
            displayTestMenu 
                = mapM_ displayTestMenu' testMenu
            displayTestMenu' ( i, t ) 
                = putStrLn $ ( if i == show nextID 
                                then "> " 
                                else "  "
                             ) ++ i ++ ". " ++ testName t

runTest :: String -> IO (String, Bool)
runTest i = do
    let t = fromJust $ lookup i testMenu 
    printSummary t
    waitForReturn
    testAction t
    r <- confirmResults t
    return (testID t, r)

defaultSuccessConfirmResults = do
    putStr "\n"
    putStr "[Y/n] "
    hFlush stdout
    r <- getLine
    case r of
        ""  -> return True
        "y" -> return True
        "Y" -> return True
        "n" -> return False

data Test = Test
    { testName :: String
    , testID :: String
    , testAction :: IO ()
    , printSummary :: IO ()
    , confirmResults :: IO Bool
    }

allTests 
    = [ reserveOutputTest 
      , displayBoundsTest0
      , displayBoundsTest1
      , displayBoundsTest2
      , displayBoundsTest3
      , unicodeSingleWidth0
      , unicodeSingleWidth1
      , unicodeDoubleWidth0
      , unicodeDoubleWidth1
      , attributesTest0
      , attributesTest1
      , attributesTest2
      , attributesTest3
      , attributesTest4
      , attributesTest5
      , inlineTest0
      , inlineTest1
      , inlineTest2
      , cursorHideTest0
      , vertCropTest0
      , vertCropTest1
      , vertCropTest2
      , vertCropTest3
      , horizCropTest0
      , horizCropTest1
      , horizCropTest2
      , horizCropTest3
      , layer0
      , layer1
      ]

reserveOutputTest = Test 
    { testName = "Initialize and reserve terminal output then restore previous state."
    , testID = "reserveOutputTest"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        putStrLn "Line 1"
        putStrLn "Line 2"
        putStrLn "Line 3"
        putStrLn "Line 4 (press return)"
        hFlush stdout
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = do
        putStr $ [s|
Once return is pressed:
    0. The screen will be cleared. 
    1. Four lines of text should be visible.
    1. The cursor should be visible and at the start of the fifth line.

After return is pressed for the second time this test then:
    * The screen containing the test summary should be restored;
    * The cursor is visible.
|]
    , confirmResults = do
        putStr $ [s|
Did the test output match the description?
|]
        defaultSuccessConfirmResults
    }

displayBoundsTest0 = Test
    { testName = "Verify display bounds are correct test 0: Using spaces."
    , testID = "displayBoundsTest0"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        (w,h) <- displayBounds t
        let row0 = replicate (fromEnum w) 'X' ++ "\n"
            rowH = replicate (fromEnum w - 1) 'X'
            rowN = "X" ++ replicate (fromEnum w - 2) ' ' ++ "X\n"
            image = row0 ++ (concat $ replicate (fromEnum h - 2) rowN) ++ rowH
        putStr image
        hFlush stdout
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = displayBoundsTestSummary True
    , confirmResults = genericOutputMatchConfirm
    }

displayBoundsTest1 = Test
    { testName = "Verify display bounds are correct test 0: Using cursor movement."
    , testID = "displayBoundsTest1"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        (w,h) <- displayBounds t
        setCursorPos t 0 0
        let row0 = replicate (fromEnum w) 'X' ++ "\n"
        putStr row0
        forM_ [1 .. h - 2] $ \y -> do
            setCursorPos t 0 y
            putStr "X"
            hFlush stdout
            setCursorPos t (w - 1) y
            putStr "X"
            hFlush stdout
        setCursorPos t 0 (h - 1)
        let rowH = replicate (fromEnum w - 1) 'X'
        putStr rowH
        hFlush stdout
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = displayBoundsTestSummary True
    , confirmResults = genericOutputMatchConfirm
    }

displayBoundsTest2 = Test
    { testName = "Verify display bounds are correct test 0: Using Image ops."
    , testID = "displayBoundsTest2"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        bounds@(w,h) <- displayBounds t
        let firstRow = horizCat $ replicate (fromEnum w) (char defAttr 'X')
            middleRows = vertCat $ replicate (fromEnum h - 2) middleRow
            middleRow = (char defAttr 'X') <|> backgroundFill (w - 2) 1 <|> (char defAttr 'X')
            endRow = firstRow
            image = firstRow <-> middleRows <-> endRow
            pic = (picForImage image) { picCursor = Cursor (w - 1) (h - 1) }
        d <- displayContext t bounds
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = displayBoundsTestSummary True
    , confirmResults = genericOutputMatchConfirm
    }

displayBoundsTest3 = Test
    { testName = "Verify display bounds are correct test 0: Hide cursor; Set cursor pos."
    , testID = "displayBoundsTest3"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        (w,h) <- displayBounds t
        hideCursor t
        setCursorPos t 0 0
        let row0 = replicate (fromEnum w) 'X'
        putStrLn row0
        forM_ [1 .. h - 2] $ \y -> do
            setCursorPos t 0 y
            putStr "X"
            hFlush stdout
            setCursorPos t (w - 1) y
            putStr "X"
            hFlush stdout
        setCursorPos t 0 (h - 1)
        let rowH = row0
        putStr rowH
        hFlush stdout
        getLine
        showCursor t
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = displayBoundsTestSummary False
    , confirmResults = genericOutputMatchConfirm
    }

displayBoundsTestSummary hasCursor = do
    putStr $ [s|
Once return is pressed:
    0. The screen will be cleared.
|]
    if hasCursor
        then putStr "    1. The cursor will be visible."
        else putStr "    1. The cursor will NOT be visible."
    putStr [s|

    2. The border of the display will be outlined in Xs. 
       So if - and | represented the edge of the terminal window:
         |-------------|
         |XXXXXXXXXXXXX|
         |X           X||]

    if hasCursor
        then putStr $ [s|

         |XXXXXXXXXXXXC| |]
        else putStr $ [s|

         |XXXXXXXXXXXXX| |]

    putStr $ [s|

         |-------------|

        ( Where C is the final position of the cursor. There may be an X drawn
        under the cursor. )
    3. The display will remain in this state until return is pressed again.

After return is pressed for the second time:
    0. The screen containing the test summary should be restored.
    1. The cursor should be visible.
|]

genericOutputMatchConfirm = do
    putStr $ [s|
Did the test output match the description?
|]
    defaultSuccessConfirmResults

-- Explicitely definethe bytes that encode each example text.
-- This avoids any issues with how the compiler represents string literals.
--
-- This document is UTF-8 encoded so the UTF-8 string is still included for
-- reference
--
-- It's assumed the compiler will at least not barf on UTF-8 encoded text in
-- comments ;-)
--
-- txt0 = ↑↑↓↓←→←→BA

utf8Txt0 :: [[Word8]]
utf8Txt0 = [ [ 0xe2 , 0x86 , 0x91 ]
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

iso10646Txt0 :: String
iso10646Txt0 = map toEnum
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

unicodeSingleWidth0 = Test
    { testName = "Verify terminal can display unicode single-width characters. (Direct UTF-8)"
    , testID = "unicodeSingleWidth0"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        hideCursor t
        withArrayLen (concat utf8Txt0) (flip $ hPutBuf stdout)
        hPutStr stdout "\n"
        hPutStr stdout "0123456789\n"
        hFlush stdout
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = unicodeSingleWidthSummary
    , confirmResults = genericOutputMatchConfirm
    }

unicodeSingleWidth1 = Test
    { testName = "Verify terminal can display unicode single-width characters. (Image ops)"
    , testID = "unicodeSingleWidth1"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = line0 <-> line1
            line0 = iso10646String defAttr iso10646Txt0
            line1 = string defAttr "0123456789"
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = unicodeSingleWidthSummary
    , confirmResults = genericOutputMatchConfirm
    }

unicodeSingleWidthSummary = putStr [s|
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
utf8Txt1 :: [[Word8]]
utf8Txt1 = [ [0xe4,0xbd,0xa0]
           , [0xe5,0xa5,0xbd]
           , [0xe5,0x90,0x97]
           ]

iso10646Txt1 :: String
iso10646Txt1 = map toEnum [20320,22909,21527]

unicodeDoubleWidth0 = Test
    { testName = "Verify terminal can display unicode double-width characters. (Direct UTF-8)"
    , testID = "unicodeDoubleWidth0"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        hideCursor t
        withArrayLen (concat utf8Txt1) (flip $ hPutBuf stdout)
        hPutStr stdout "\n"
        hPutStr stdout "012345\n"
        hFlush stdout
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = unicodeDoubleWidthSummary
    , confirmResults = genericOutputMatchConfirm
    }

unicodeDoubleWidth1 = Test
    { testName = "Verify terminal can display unicode double-width characters. (Image ops)"
    , testID = "unicodeDoubleWidth1"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = line0 <-> line1
            line0 = iso10646String defAttr iso10646Txt1
            line1 = string defAttr "012345"
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = unicodeDoubleWidthSummary
    , confirmResults = genericOutputMatchConfirm
    }

unicodeDoubleWidthSummary = putStr [s|
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

allColors = zip [ black, red, green, yellow, blue, magenta, cyan, white ]
                [ "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white" ]

allBrightColors 
    = zip [ brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite ]
          [ "bright black", "bright red", "bright green", "bright yellow", "bright blue", "bright magenta", "bright cyan", "bright white" ]

attributesTest0 = Test 
    { testName = "Character attributes: foreground colors."
    , testID = "attributesTest0"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = border <|> column0 <|> border <|> column1 <|> border
            column0 = vertCat $ map lineWithColor allColors
            border = vertCat $ replicate (length allColors) $ string defAttr " | "
            column1 = vertCat $ map (string defAttr . snd) allColors
            lineWithColor (c, cName) = string (defAttr `withForeColor` c) cName
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = do
        putStr $ [s|
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
    , confirmResults = do
        putStr $ [s|
Did the test output match the description?
|]
        defaultSuccessConfirmResults
    }

attributesTest1 = Test 
    { testName = "Character attributes: background colors."
    , testID = "attributesTest1"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = border <|> column0 <|> border <|> column1 <|> border
            column0 = vertCat $ map lineWithColor allColors
            border = vertCat $ replicate (length allColors) $ string defAttr " | "
            column1 = vertCat $ map (string defAttr . snd) allColors
            lineWithColor (c, cName) = string (defAttr `withBackColor` c) cName
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = do
        putStr $ [s|
Once return is pressed:
    0. The screen will be cleared.
    1. The cursor will be hidden.
    2. 9 lines of text in two columns will be drawn. The first column will
    contain be a name of a standard color for an 8 color terminal rendered with
    the default foreground color with a background the named color.  For
    instance, one line will contain be the word "magenta" and the word should
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
    , confirmResults = do
        putStr $ [s|
Did the test output match the description?
|]
        defaultSuccessConfirmResults
    }

attributesTest2 = Test 
    { testName = "Character attributes: Vivid foreground colors."
    , testID = "attributesTest2"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = horizCat [border, column0, border, column1, border, column2, border]
            border = vertCat $ replicate (length allColors) $ string defAttr " | "
            column0 = vertCat $ map lineWithColor0 allColors
            column1 = vertCat $ map lineWithColor1 allBrightColors
            column2 = vertCat $ map (string defAttr . snd) allColors
            lineWithColor0 (c, cName) = string (defAttr `withForeColor` c) cName
            lineWithColor1 (c, cName) = string (defAttr `withForeColor` c) cName
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = do
        putStr $ [s|
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
    , confirmResults = do
        putStr $ [s|
Did the test output match the description?
|]
        defaultSuccessConfirmResults
    }

attributesTest3 = Test 
    { testName = "Character attributes: Vivid background colors."
    , testID = "attributesTest3"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = horizCat [border, column0, border, column1, border, column2, border]
            border = vertCat $ replicate (length allColors) $ string defAttr " | "
            column0 = vertCat $ map lineWithColor0 allColors
            column1 = vertCat $ map lineWithColor1 allBrightColors
            column2 = vertCat $ map (string defAttr . snd) allColors
            lineWithColor0 (c, cName) = string (defAttr `withBackColor` c) cName
            lineWithColor1 (c, cName) = string (defAttr `withBackColor` c) cName
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = do
        putStr $ [s|
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
        
For instance, one line will contain be the word "magenta" and the word should
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
    , confirmResults = do
        putStr $ [s|
Did the test output match the description?
|]
        defaultSuccessConfirmResults
    }

attrCombos = 
    [ ( "default", id )
    , ( "bold", flip withStyle bold )
    , ( "blink", flip withStyle blink )
    , ( "underline", flip withStyle underline )
    , ( "bold + blink", flip withStyle (bold + blink) )
    , ( "bold + underline", flip withStyle (bold + underline) )
    , ( "underline + blink", flip withStyle (underline + blink) )
    , ( "bold + blink + underline", flip withStyle (bold + blink + underline) )
    ]

attributesTest4 = Test 
    { testName = "Character attributes: Bold; Blink; Underline."
    , testID = "attributesTest4"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = horizCat [border, column0, border, column1, border]
            border = vertCat $ replicate (length attrCombos) $ string defAttr " | "
            column0 = vertCat $ map lineWithAttrs attrCombos
            column1 = vertCat $ map (string defAttr . fst) attrCombos
            lineWithAttrs (desc, attrF) = string (attrF defAttr) desc
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = do
        putStr $ [s|
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
    , confirmResults = do
        putStr $ [s|
Did the test output match the description?
|]
        defaultSuccessConfirmResults
    }

attributesTest5 = Test 
    { testName = "Character attributes: 240 color palette"
    , testID = "attributesTest5"
    , testAction = do
        t <- outputForCurrentTerminal
        reserveDisplay t
        let pic = picForImage image
            image = vertCat $ map horizCat $ splitColorImages colorImages
            colorImages = map (\i -> string (currentAttr `withBackColor` Color240 i) " ") [0..239]
            splitColorImages [] = []
            splitColorImages is = (take 20 is ++ [string defAttr " "]) : (splitColorImages (drop 20 is))
        d <- displayBounds t >>= displayContext t
        outputPicture d pic
        getLine
        releaseDisplay t
        releaseTerminal t
        return ()
    , printSummary = do
        putStr $ [s|
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
    , confirmResults = do
        putStr $ [s|
Did the test output match the description?
|]
        defaultSuccessConfirmResults
    }

inlineTest0 = Test
    { testName = "Verify styled output can be performed without clearing the screen."
    , testID = "inlineTest0"
    , testAction = do
        putStrLn "line 1."
        putAttrChange_ $ backColor red >> applyStyle underline
        putStrLn "line 2."
        putAttrChange_ $ defaultAll
        putStrLn "line 3."
    , printSummary = putStr $ [s|
lines are in order.
The second line "line 2" should have a red background and the text underline.
The third line "line 3" should be drawn in the same style as the first line.
|]

    , confirmResults = genericOutputMatchConfirm
    }

inlineTest1 = Test
    { testName = "Verify styled output can be performed without clearing the screen."
    , testID = "inlineTest1"
    , testAction = do
        putStr "Not styled. "
        putAttrChange_ $ backColor red >> applyStyle underline
        putStr " Styled! "
        putAttrChange_ $ defaultAll
        putStrLn "Not styled."
    , printSummary = putStr $ [s|
|]

    , confirmResults = genericOutputMatchConfirm
    }

inlineTest2 = Test
    { testName = "Verify styled output can be performed without clearing the screen."
    , testID = "inlineTest1"
    , testAction = do
        putStr "Not styled. "
        putAttrChange_ $ backColor red >> applyStyle underline
        putStr " Styled! "
        putAttrChange_ $ defaultAll
        putStr "Not styled.\n"
    , printSummary = putStr $ [s|
|]
    , confirmResults = genericOutputMatchConfirm
    }

cursorHideTest0 :: Test
cursorHideTest0 = Test
    { testName = "Verify the cursor is hid and re-shown. issue #7"
    , testID = "cursorHideTest0"
    , testAction = do
        vty <- mkVty def
        showCursor $ outputIface vty
        setCursorPos (outputIface vty) 5 5
        nextEvent vty
        hideCursor $ outputIface vty
        nextEvent vty
        shutdown vty
        return ()
    , printSummary = putStr $ [s|
    1. verify the cursor is displayed.
    2. press enter
    3. verify the cursor is hid.
    4. press enter.
    5. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

outputImageAndWait :: Image -> IO ()
outputImageAndWait image = do
    let pic = picForImage image
    outputPicAndWait pic

outputPicAndWait :: Picture -> IO ()
outputPicAndWait pic = do
    t <- outputForCurrentTerminal
    reserveDisplay t
    d <- displayBounds t >>= displayContext t
    outputPicture d pic
    getLine
    releaseDisplay t
    releaseTerminal t
    return ()
    
vertCropTest0 :: Test
vertCropTest0 = Test
    { testName = "Verify bottom cropping works as expected with single column chars"
    , testID = "cropTest0"
    , testAction = do
        let block0 = cropBottom 2 $ vertCat $ map (string defAttr) lorumIpsum
            block1 = vertCat $ map (string defAttr) $ take 2 lorumIpsum
            image = block0 <-> backgroundFill 10 2 <-> block1
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the two text blocks are identical.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

vertCropTest1 :: Test
vertCropTest1 = Test
    { testName = "Verify bottom cropping works as expected with double column chars"
    , testID = "cropTest0"
    , testAction = do
        let block0 = cropBottom 2 $ vertCat $ map (string defAttr) lorumIpsumChinese
            block1 = vertCat $ map (string defAttr) $ take 2 lorumIpsumChinese
            image = block0 <-> backgroundFill 10 2 <-> block1
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the two text blocks are identical.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

vertCropTest2 :: Test
vertCropTest2 = Test
    { testName = "Verify top cropping works as expected with single column chars"
    , testID = "cropTest2"
    , testAction = do
        let block0 = cropTop 2 $ vertCat $ map (string defAttr) lorumIpsum
            block1 = vertCat $ map (string defAttr) $ drop (length lorumIpsum - 2) lorumIpsum
            image = block0 <-> backgroundFill 10 2 <-> block1
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the two text blocks are identical.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

vertCropTest3 :: Test
vertCropTest3 = Test
    { testName = "Verify top cropping works as expected with double column chars"
    , testID = "cropTest0"
    , testAction = do
        let block0 = cropTop 2 $ vertCat $ map (string defAttr) lorumIpsumChinese
            block1 = vertCat $ map (string defAttr) $ drop (length lorumIpsumChinese - 2 ) lorumIpsumChinese
            image = block0 <-> backgroundFill 10 2 <-> block1
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the two text blocks are identical.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

horizCropTest0 :: Test
horizCropTest0 = Test
    { testName = "Verify right cropping works as expected with single column chars"
    , testID = "cropTest0"
    , testAction = do
        let baseImage = vertCat $ map (string defAttr) lorumIpsum
            croppedImage = cropRight (imageWidth baseImage `div` 2) baseImage 
            image = baseImage <-> backgroundFill 10 2 <-> croppedImage
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the bottom text block is about half the width of the top text block.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

horizCropTest1 :: Test
horizCropTest1 = Test
    { testName = "Verify right cropping works as expected with double column chars"
    , testID = "cropTest0"
    , testAction = do
        let baseImage = vertCat $ map (string defAttr) lorumIpsumChinese
            croppedImage = cropRight (imageWidth baseImage `div` 2) baseImage 
            image = baseImage <-> backgroundFill 10 2 <-> croppedImage
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the bottom text block is the left half of the top block. Ellipses on the right edge are OK.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

horizCropTest2 :: Test
horizCropTest2 = Test
    { testName = "Verify left cropping works as expected with single column chars"
    , testID = "cropTest0"
    , testAction = do
        let baseImage = vertCat $ map (string defAttr) lorumIpsum
            croppedImage = cropLeft (imageWidth baseImage `div` 2) baseImage 
            image = baseImage <-> backgroundFill 10 2 <-> croppedImage
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the bottom text block is the right half of the top text block.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

horizCropTest3 :: Test
horizCropTest3 = Test
    { testName = "Verify right cropping works as expected with double column chars"
    , testID = "cropTest0"
    , testAction = do
        let baseImage = vertCat $ map (string defAttr) lorumIpsumChinese
            croppedImage = cropLeft (imageWidth baseImage `div` 2) baseImage 
            image = baseImage <-> backgroundFill 10 2 <-> croppedImage
        outputImageAndWait image
    , printSummary = putStr $ [s|
    1. Verify the bottom text block is the right half of the top block. Ellipses on the left edge are OK.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

layer0 :: Test
layer0 = Test
    { testName = "verify layer 0"
    , testID = "layer0"
    , testAction = do
        let upperImage = vertCat $ map (string defAttr) lorumIpsumChinese
            lowerImage = vertCat $ map (string defAttr) lorumIpsum
            p = picForLayers [upperImage, lowerImage]
        outputPicAndWait p
    , printSummary = putStr $ [s|
    1. Verify the text block appears to be Chinese text placed on top Latin text.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

layer1 :: Test
layer1 = Test
    { testName = "verify layer 1"
    , testID = "layer1"
    , testAction = do
        let upperImage = vertCat $ map (string defAttr) lorumIpsumChinese
            block = resize 10 10 upperImage
            layer0 = vertCat $ map (string defAttr) lorumIpsum
            layer1 = charFill (defAttr `withBackColor` blue) '#' 1000 1000
        cheesyAnim0 block [layer0, layer1]
    , printSummary = putStr $ [s|
    1. Verify the text block appears to be Chinese text moving on top a Latin text.
       Which is all on a background of '#' characters over blue.
    2. press enter.
    3. the display should return to the state before the test.
|]
    , confirmResults = genericOutputMatchConfirm
    }

cheesyAnim0 :: Image -> [Image] -> IO ()
cheesyAnim0 i background = do
    t <- outputForCurrentTerminal
    reserveDisplay t
    bounds <- displayBounds t
    d <- displayContext t bounds
    forM_ [0..100] $ \t -> do
        let i_offset = translate (t `mod` fst bounds)
                                 (t `div` 2 `mod` snd bounds)
                                 i
        let pic = picForLayers $ i_offset : background
        outputPicture d pic
        threadDelay 50000
    releaseDisplay t
    releaseTerminal t
    return ()

lorumIpsum :: [String]
lorumIpsum = lines [s|
Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium,
totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae
dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit,
sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam
est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius
modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima
veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea
commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil
molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?
|]

lorumIpsumChinese :: [String]
lorumIpsumChinese = lines [s|
輐銛 螷蟞覮 裌覅詵 暕 鴅噮 槶 惝掭掝 婸媥媕 耏胠臿, 汫汭沎 忕汌卣 蚡袀 僣 蒮 瀁瀎瀊 渮湸湤 緌翢,
腠腶舝 糲蘥蠩 樏殣氀 蒮 蹢鎒 滍 鸄齴 櫧櫋瀩 鬄鵊鵙 莃荶衒, 毸溠 橀 簎艜薤 莃荶衒 翣聜蒢
斔櫅檷 晛桼桾 拻敁柧 犿玒 膣, 墐 笓粊紒 bacon 鼀齕, 蔝蓶蓨 顊顃餭 姴怤 骱 暕 蹢鎒鎛 藒襓謥 鄻鎟霣
鬎鯪, 鐩闤 硻禂稢 谾踘遳 撱 赲 迡 箷 蛃袚觙 萇雊蜩 壿嫷 鋡 縢羱聬 跐鉠鉣 蔝蓶蓨 匢奾灱 溮煡煟 雥齆犪
蔰 虈觿, 腷腯葹 鍹餳駷 蛚袲褁蜸 皯竻 瀁瀎 蜭蜸覟 梪涫湴 揗斝湁 毼
|]

