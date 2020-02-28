module Main where

import System.Exit (exitFailure)

import Graphics.Vty.UnicodeWidthTable.Types
import Graphics.Vty.UnicodeWidthTable.IO
import Graphics.Vty.UnicodeWidthTable.Query

main :: IO ()
main = do
    let r1 = WidthTableRange 0 100 1
        r2 = WidthTableRange 101 200 2
        t = UnicodeWidthTable { unicodeWidthTableRanges = [r1, r2]
                              }

    writeUnicodeWidthTable "testfile.dat" t
    result <- readUnicodeWidthTable "testfile.dat"

    case result of
        Left msg -> do
            putStrLn $ "Error reading table: " <> msg
            exitFailure
        Right t2 -> do
            putStrLn "Original table:"
            print t
            putStrLn "New table:"
            print t2

    builtTable <- buildUnicodeWidthTable
    print builtTable
    writeUnicodeWidthTable "terminal_widths.dat" builtTable
