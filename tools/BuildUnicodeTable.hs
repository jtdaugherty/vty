module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Console.GetOpt

import Graphics.Vty.Config (terminalWidthTablePath)
import Graphics.Vty.UnicodeWidthTable.Types
import Graphics.Vty.UnicodeWidthTable.IO
import Graphics.Vty.UnicodeWidthTable.Query

data Arg = Help
         | OutputPath String
         deriving (Eq, Show)

options :: Config -> [OptDescr Arg]
options config =
    [ Option "h" ["help"] (NoArg Help)
      "This help output"
    , Option "p" ["path"] (ReqArg OutputPath "PATH")
      ("The output path to write to (default: " <> fromMaybe "<none>" (configOutputPath config) <> ")")
    ]

data Config =
    Config { configOutputPath :: Maybe FilePath
           }
           deriving (Show)

mkDefaultConfig :: IO Config
mkDefaultConfig = do
    Config <$> terminalWidthTablePath

usage :: IO ()
usage = do
    config <- mkDefaultConfig
    pn <- getProgName
    putStrLn $ "Usage: " <> pn <> " [options]"
    putStrLn ""
    putStrLn "This tool queries the terminal on stdout to determine the widths"
    putStrLn "of Unicode characters rendered to the terminal. The resulting data"
    putStrLn "is written to a table at the specified output path for later"
    putStrLn "loading by Vty-based applications."
    putStrLn ""

    putStrLn $ usageInfo pn (options config)

configFromArgs :: [Arg] -> Config -> Config
configFromArgs [] c =
    c
configFromArgs (Help:as) c =
    configFromArgs as c
configFromArgs (OutputPath p:as) c =
    configFromArgs as (c { configOutputPath = Just p })

main :: IO ()
main = do
    defConfig <- mkDefaultConfig
    strArgs <- getArgs
    let (args, unused, errors) = getOpt Permute (options defConfig) strArgs

    when (not $ null errors) $ do
        mapM_ putStrLn errors
        exitFailure

    when ((not $ null unused) || (Help `elem` args)) $ do
        usage
        exitFailure

    let config = configFromArgs args defConfig

    outputPath <- case configOutputPath config of
        Nothing -> do
            putStrLn "Error: could not obtain terminal width table path"
            exitFailure
        Just path -> return path

    putStrLn "Querying terminal:"
    builtTable <- buildUnicodeWidthTable
    writeUnicodeWidthTable outputPath builtTable
    putStrLn $ "\nOutput table written to " <> outputPath
