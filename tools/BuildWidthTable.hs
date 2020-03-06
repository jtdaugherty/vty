{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Control.Exception as E
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

import Graphics.Vty.Config ( terminalWidthTablePath, currentTerminalName
                           , vtyConfigPath, addConfigWidthMap
                           , ConfigUpdateResult(..)
                           )
import Graphics.Vty.UnicodeWidthTable.IO
import Graphics.Vty.UnicodeWidthTable.Query

data Arg = Help
         | OutputPath String
         | TableUpperBound String
         | UpdateConfig
         | VtyConfigPath String
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
    , Option "u" ["update-config"] (NoArg UpdateConfig)
      "Create or update the Vty configuration file to use the new table (default: no)"
    , Option "c" ["config-path"] (ReqArg VtyConfigPath "PATH")
      ("Update the specified Vty configuration file path when -u is set (default: " <>
       configPath config <> ")")
    ]

data Config =
    Config { configOutputPath :: Maybe FilePath
           , configBound :: Char
           , configUpdate :: Bool
           , configPath :: FilePath
           }
           deriving (Show)

mkDefaultConfig :: IO Config
mkDefaultConfig = do
    Config <$> terminalWidthTablePath
           <*> pure defaultUnicodeTableUpperBound
           <*> pure False
           <*> vtyConfigPath

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
updateConfigFromArg UpdateConfig c =
    c { configUpdate = True }
updateConfigFromArg (VtyConfigPath p) c =
    c { configPath = p }
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

    putStrLn "Querying terminal:"
    builtTable <- buildUnicodeWidthTable $ configBound config

    let dir = takeDirectory outputPath
    createDirectoryIfMissing True dir
    writeUnicodeWidthTable outputPath builtTable

    putStrLn $ "\nOutput table written to " <> outputPath

    when (configUpdate config) $ do
        let cPath = configPath config
        Just tName <- currentTerminalName

        result <- E.try $ addConfigWidthMap cPath tName outputPath

        case result of
            Left (e::E.SomeException) -> do
                putStrLn $ "Error updating Vty configuration at " <> cPath <> ": " <>
                           show e
                exitFailure
            Right ConfigurationCreated -> do
                putStrLn $ "Configuration file created: " <> cPath
            Right ConfigurationModified -> do
                putStrLn $ "Configuration file updated: " <> cPath
            Right (ConfigurationConflict other) -> do
                putStrLn $ "Configuration file not updated: uses a different table " <>
                           "for TERM=" <> tName <> ": " <> other
            Right ConfigurationRedundant -> do
                putStrLn $ "Configuration file not updated: configuration " <>
                           cPath <> " already uses table " <> outputPath <>
                           " for TERM=" <> tName
