module Main where

import System.Console.ANSI
import System.IO
import System.Environment (getArgs)
import Data.List
import Data.Traversable
import Graphics.Text.Width (wcswidth)
import Data.Char
import Data.Foldable
import Text.Printf

foreign import ccall unsafe "is_emoji" isEmoji :: Char -> Bool
foreign import ccall unsafe "is_extended_pictographic" isExtendedPictographic :: Char -> Bool

shouldConsider c =
  case generalCategory c of
    Control     -> False
    NotAssigned -> False
    Surrogate   -> False
    _           -> True

parseArgs [] = (False, False)
parseArgs ["--only-pictographics"] = (True, False)
parseArgs ["--only-inconsistencies"] = (False, True)
parseArgs ["--only-pictographics", "--only-inconsistencies"] = (True, True)
parseArgs ["--only-inconsistencies", "--only-pictographics"] = (True, True)
parseArgs _ = error "Bad arguments: --only-pictographics --only-inconsistencies supported, only once."

charWidth :: String -> IO Int
charWidth cs@(c:_) =
  do printf "\r%06x: " (ord c)
     mapM_ putChar cs
     Just (_row, col) <- getCursorPosition0
     return (col - 8)

checkString :: Bool -> Handle -> String -> IO ()
checkString onlyInconsistencies h cs@(c:_) = do
  width <- if shouldConsider c then charWidth cs else return 0
  let widthStr = if shouldConsider c then show width else "-"
  let reportWidth = do
       mapM_ (hPrintf h "%06x " . ord) cs
       hPrintf h "\t%s" widthStr
  if onlyInconsistencies
    then if (width /= wcswidth cs) && shouldConsider c
      then do
        reportWidth
        hPrintf h " /= %d\n" (wcswidth cs)
      else pure ()
    else do
      reportWidth
      hPrintf h "\n"

main :: IO ()
main = do
  args <- getArgs
  let (onlyPictographics, onlyInconsistencies) = parseArgs args
  let filterFn = if onlyPictographics then isExtendedPictographic else const True
  withFile "output.txt" WriteMode $ \h ->
    do hSetBuffering stdout NoBuffering
       for_ (filter filterFn ['\0'..'\x2FFFF']) $ \i -> do
         checkString onlyInconsistencies h [i]
         if isEmoji i
           then do
             checkString onlyInconsistencies h [i, '\xFE0E']
             checkString onlyInconsistencies h [i, '\xFE0F']
           else pure ()

