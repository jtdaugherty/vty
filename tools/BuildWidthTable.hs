{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.FilePath (takeDirectory)
import System.Exit (exitFailure)
import System.Console.GetOpt
import Text.Read (readMaybe)

import Graphics.Vty.Config (terminalWidthTablePath, currentTerminalName, vtyConfigPath)
import Graphics.Vty.UnicodeWidthTable.IO
import Graphics.Vty.UnicodeWidthTable.Query

data Arg = Help
         | OutputPath String
         | TableUpperBound String
         deriving (Eq, Show)

options :: Config -> [OptDescr Arg]
options config =
    [ Option "h" ["help"] (NoArg Help)
      "This help output"
    , Option "b" ["bound"] (ReqArg TableUpperBound "MAX_CHAR")
      ("The maximum Unicode code point to test when building the table " <>
       "(default: " <> (show $ fromEnum $ configBound config) <> ")")
    , Option "p" ["path"] (ReqArg OutputPath "PATH")
      ("The output path to write to (default: " <>
       fromMaybe "<none>" (configOutputPath config) <> ")")
    ]

data Config =
    Config { configOutputPath :: Maybe FilePath
           , configBound :: Char
           }
           deriving (Show)

mkDefaultConfig :: IO Config
mkDefaultConfig = do
    Config <$> terminalWidthTablePath
           <*> pure defaultUnicodeTableUpperBound

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

updateConfigFromArg :: Arg -> Config -> Config
updateConfigFromArg Help c =
    c
updateConfigFromArg (TableUpperBound s) c =
    case readMaybe s of
        Nothing -> error $ "Invalid table upper bound: " <> show s
        Just v  -> c { configBound = toEnum v }
updateConfigFromArg (OutputPath p) c =
    c { configOutputPath = Just p }

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

    let config = foldr updateConfigFromArg defConfig args

    outputPath <- case configOutputPath config of
        Nothing -> do
            putStrLn "Error: could not obtain terminal width table path"
            exitFailure
        Just path -> return path

    let dir = takeDirectory outputPath
    createDirectoryIfMissing True dir

    putStrLn "Querying terminal:"
    builtTable <- buildUnicodeWidthTable $ configBound config
    writeUnicodeWidthTable outputPath builtTable
    putStrLn $ "\nOutput table written to " <> outputPath

    Just tName <- currentTerminalName
    configPath <- vtyConfigPath
    putStrLn ""
    putStrLn "To configure your Vty-based applications to use this map, add"
    putStrLn $ "the following line to " <> configPath <> ":"
    putStrLn ""
    putStrLn $ "  widthMap " <> show tName <> " " <> show outputPath
    putStrLn ""
