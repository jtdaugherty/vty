module Main where

import System.Console.ANSI

import System.IO

import Control.Concurrent
import Control.Monad


examples :: [IO ()]
examples = [cursorMovementExample,
            lineChangeExample,
            setPositionExample,
            clearExample,
            scrollExample,
            sgrExample,
            cursorVisibilityExample]

main :: IO ()
main = mapM_ (\example -> resetScreen >> example) examples

resetScreen :: IO ()
resetScreen = clearScreen >> setSGR Reset >> setPosition 1 1

pause :: IO ()
pause = do
    hFlush stdout
    threadDelay 1000000

cursorMovementExample :: IO ()
cursorMovementExample = do
    putStrLn "Line One"
    putStr "Line Two"
    pause
    -- Line One
    -- Line Two
    
    cursorUp 1
    putStr " - Extras"
    pause
    -- Line One - Extras
    -- Line Two
    
    cursorBackward 2
    putStr "zz"
    pause
    -- Line One - Extrzz
    -- Line Two
    
    cursorForward 2
    putStr "- And More"
    pause
    -- Line One - Extrzz  - And More
    -- Line Two
    
    cursorDown 1
    putStr "Disconnected"
    pause
    -- Line One - Extrzz  - AndMore
    -- Line Two                    Disconnected

lineChangeExample :: IO ()
lineChangeExample = do
    putStrLn "Line One"
    putStr "Line Two"
    pause
    -- Line One
    -- Line Two
    
    previousLine 1
    putStr "New Line One"
    pause
    -- New Line One
    -- Line Two
    
    nextLine 1
    putStr "New Line Two"
    pause
    -- New Line One
    -- New Line Two

setPositionExample :: IO ()
setPositionExample = do
    putStrLn "Line One"
    putStrLn "Line Two"
    pause
    -- Line One
    -- Line Two
    
    setPosition 1 6
    putStr "Foo"
    pause
    -- Line Foo
    -- Line Two
    
    setPosition 2 6
    putStr "Bar"
    pause
    -- Line Foo
    -- Line Bar
    
    setColumn 2
    putStr "oaf"
    pause
    -- Line Foo
    -- Loaf Bar

clearExample :: IO ()
clearExample = do
    putStrLn "Line One"
    putStrLn "Line Two"
    pause
    -- Line One
    -- Line Two
    
    setPosition 1 5
    clearFromCursorToScreenEnd
    pause
    -- Line
    
    
    resetScreen
    putStrLn "Line One"
    putStrLn "Line Two"
    pause
    -- Line One
    -- Line Two
    
    setPosition 2 5
    clearFromCursorToScreenBeginning
    pause
    --
    --     Two
    
    
    resetScreen
    putStrLn "Line One"
    putStrLn "Line Two"
    pause
    -- Line One
    -- Line Two
    
    setPosition 1 5
    clearFromCursorToLineEnd
    pause
    -- Line
    -- Line Two
    
    setPosition 2 5
    clearFromCursorToLineBeginning
    pause
    -- Line
    --      Two
    
    clearLine
    pause
    -- Line
    
    clearScreen
    pause
    --

scrollExample :: IO ()
scrollExample = do
    putStrLn "Line One"
    putStrLn "Line Two"
    putStrLn "Line Three"
    pause
    -- Line One
    -- Line Two
    -- Line Three
    
    scrollPageDown 2
    pause
    --
    --
    -- Line One
    -- Line Two
    -- Line Three
    
    scrollPageUp 3
    pause
    -- Line Two
    -- Line Three

sgrExample :: IO ()
sgrExample = do
    let colors = enumFromTo minBound maxBound :: [ANSIColor]
    forM_ [ForegroundNormalIntensity, ForegroundHighIntensity, BackgroundNormalIntensity, BackgroundHighIntensity] $ \color_way -> do
        resetScreen
        forM_ colors $ \color -> do
            setSGR Reset
            setSGR (color_way color)
            putStrLn (show color)
        pause
    -- All the colors, 4 times in sequence
    
    let named_styles = [ (BoldIntensity, "Bold")
                       , (FaintIntensity, "Faint")
                       , (NormalIntensity, "Normal")
                       , (Italic, "Italic")
                       , (SingleUnderline, "Single Underline")
                       , (DoubleUnderline, "Double Underline")
                       , (NoUnderline, "No Underline")
                       , (SlowBlink, "Slow Blink")
                       , (RapidBlink, "Rapid Blink")
                       , (NoBlink, "No Blink")
                       , (Conceal, "Conceal")
                       , (Reveal, "Reveal")
                       ]
    forM_ named_styles $ \(style, name) -> do
              resetScreen
              setSGR style
              putStrLn name
              pause
    -- Text describing a style displayed in that style in sequence
    
    setSGR (ForegroundHighIntensity Red)
    setSGR (BackgroundHighIntensity Blue)
    
    clearScreen >> setPosition 1 1
    setSGR DontSwapForegroundBackground
    putStr "Red-On-Blue"
    pause
    -- Red-On-Blue
    
    clearScreen >> setPosition 1 1
    setSGR SwapForegroundBackground
    putStr "Blue-On-Red"
    pause
    -- Blue-On-Red

cursorVisibilityExample :: IO ()
cursorVisibilityExample = do
    putStr "Cursor Demo"
    pause
    -- Cursor Demo|
    
    hideCursor
    pause
    -- Cursor Demo
    
    showCursor
    pause
    -- Cursor Demo|