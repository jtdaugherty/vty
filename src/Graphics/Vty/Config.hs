{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Vty supports a configuration file format and provides a
-- corresponding 'VtyUserConfig' data type. The 'VtyUserConfig' can be
-- provided to platform packages' @mkVty@ functions to customize the
-- application's use of Vty.
--
-- = Debug
--
-- == @debugLog@
--
-- Format:
--
-- @
--  \"debugLog\" string
-- @
--
-- The value of the environment variable @VTY_DEBUG_LOG@ is equivalent
-- to a debugLog entry at the end of the last config file.
--
-- = Input Processing
--
-- == @map@
--
-- Format:
--
-- @
--  \"map\" term string key modifier_list
--  where
--      key := KEsc | KChar Char | KBS ... (same as 'Key')
--      modifier_list := \"[\" modifier+ \"]\"
--      modifier := MShift | MCtrl | MMeta | MAlt
--      term := "_" | string
-- @
--
-- E.g., if the contents are
--
-- @
--  map _       \"\\ESC[B\"    KUp   []
--  map _       \"\\ESC[1;3B\" KDown [MAlt]
--  map \"xterm\" \"\\ESC[D\"    KLeft []
-- @
--
-- Then the bytes @\"\\ESC[B\"@ will result in the KUp event on all
-- terminals. The bytes @\"\\ESC[1;3B\"@ will result in the event KDown
-- with the MAlt modifier on all terminals. The bytes @\"\\ESC[D\"@ will
-- result in the KLeft event when @TERM@ is @xterm@.
--
-- If a debug log is requested then vty will output the current input
-- table to the log in the above format. A workflow for using this is
-- to set @VTY_DEBUG_LOG@. Run the application. Check the debug log for
-- incorrect mappings. Add corrected mappings to @$HOME\/.vty\/config@.
--
-- = Unicode Character Width Maps
--
-- == @widthMap@
--
-- Format:
--
-- @
--  \"widthMap\" string string
-- @
--
-- E.g.,
--
-- @
--   widthMap \"xterm\" \"\/home\/user\/.vty\/xterm\_map.dat\"
-- @
--
-- This directive specifies the path to a Unicode character
-- width map (the second argument) that should correspond to
-- the terminal named by first argument. Unicode character
-- width maps can be produced either by running platform
-- packages' width table tools or by calling the library routine
-- 'Graphics.Vty.UnicodeWidthTable.Query.buildUnicodeWidthTable'. Vty
-- platform packages should use these configuration settings to attempt
-- to load and install the specified width map.
module Graphics.Vty.Config
  ( InputMap
  , VtyUserConfig(..)
  , userConfig
  , overrideEnvConfig
  , currentTerminalName
  , runParseConfig
  , parseConfigFile
  , defaultConfig

  , vtyConfigPath
  , widthTableFilename
  , vtyDataDirectory
  , terminalWidthTablePath
  , vtyConfigFileEnvName

  , ConfigUpdateResult(..)
  , addConfigWidthMap
  )
where

import Prelude

import Control.Applicative hiding (many)

import Control.Exception (catch, IOException)
import Control.Monad (liftM, guard, void)

import qualified Data.ByteString as BS
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

import Graphics.Vty.Input.Events

import GHC.Generics

import System.Directory ( getAppUserDataDirectory, doesFileExist
                        , createDirectoryIfMissing
                        )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)

import Text.Parsec hiding ((<|>))
import Text.Parsec.Token ( GenLanguageDef(..) )
import qualified Text.Parsec.Token as P

-- | Mappings from input bytes to event in the order specified. Later
-- entries take precedence over earlier in the case multiple entries
-- have the same byte string.
type InputMap = [(Maybe String, String, Event)]

-- | A Vty core library configuration. Platform-specific details are not
-- included in the VtyUserConfig.
data VtyUserConfig =
    VtyUserConfig { configDebugLog :: Maybe FilePath
                  -- ^ Debug information is appended to this file if not
                  -- Nothing.
                  , configInputMap :: InputMap
                  -- ^ The (input byte, output event) pairs extend the internal
                  -- input table of VTY and the table from terminfo.
                  --
                  -- See "Graphics.Vty.Config" module documentation for
                  -- documentation of the @map@ directive.
                  , configTermWidthMaps :: [(String, FilePath)]
                  -- ^ Terminal width map files.
                  , configAllowCustomUnicodeWidthTables :: Maybe Bool
                  -- ^ Whether to permit custom Unicode width table loading by
                  -- 'Graphics.Vty.mkVty'. @'Just' 'False'@ indicates that
                  -- table loading should not be performed. Other values permit
                  -- table loading.
                  --
                  -- If a table load is attempted and fails, information
                  -- about the failure will be logged to the debug log if the
                  -- configuration specifies one. If no custom table is loaded
                  -- (or if a load fails), the built-in character width table
                  -- will be used.
                  }
                  deriving (Show, Eq)

defaultConfig :: VtyUserConfig
defaultConfig = mempty

instance Semigroup VtyUserConfig where
    c0 <> c1 =
        -- latter config takes priority for everything but inputMap
        VtyUserConfig { configDebugLog =
                          configDebugLog c1 <|> configDebugLog c0
                      , configInputMap =
                          configInputMap c0 <> configInputMap c1
                      , configTermWidthMaps =
                          configTermWidthMaps c1 <|> configTermWidthMaps c0
                      , configAllowCustomUnicodeWidthTables =
                          configAllowCustomUnicodeWidthTables c1 <|> configAllowCustomUnicodeWidthTables c0
                      }

instance Monoid VtyUserConfig where
    mempty =
        VtyUserConfig { configDebugLog = mempty
                      , configInputMap = mempty
                      , configTermWidthMaps = []
                      , configAllowCustomUnicodeWidthTables = Nothing
                      }
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

vtyDataDirectory :: IO FilePath
vtyDataDirectory = getAppUserDataDirectory "vty"

vtyConfigPath :: IO FilePath
vtyConfigPath = do
    dir <- vtyDataDirectory
    return $ dir </> "config"

vtyConfigFileEnvName :: String
vtyConfigFileEnvName = "VTY_CONFIG_FILE"

-- | Load a configuration from 'vtyConfigPath' and @$VTY_CONFIG_FILE@.
-- If none is found, build a default configuration.
userConfig :: IO VtyUserConfig
userConfig = do
    configFile <- vtyConfigPath >>= parseConfigFile
    overrideConfig <- maybe (return defaultConfig) parseConfigFile =<<
        lookupEnv vtyConfigFileEnvName
    let base = configFile <> overrideConfig
    mappend base <$> overrideEnvConfig

widthTableFilename :: String -> String
widthTableFilename term = "width_table_" <> term <> ".dat"

termVariable :: String
termVariable = "TERM"

currentTerminalName :: IO (Maybe String)
currentTerminalName = lookupEnv termVariable

terminalWidthTablePath :: IO (Maybe FilePath)
terminalWidthTablePath = do
    dataDir <- vtyDataDirectory
    result <- lookupEnv termVariable
    case result of
        Nothing -> return Nothing
        Just term -> do
            return $ Just $ dataDir </> widthTableFilename term

overrideEnvConfig :: IO VtyUserConfig
overrideEnvConfig = do
    d <- lookupEnv "VTY_DEBUG_LOG"
    return $ defaultConfig { configDebugLog = d }

-- | Parse a Vty configuration file.
--
-- Lines in config files that fail to parse are ignored. Later entries
-- take precedence over earlier ones.
parseConfigFile :: FilePath -> IO VtyUserConfig
parseConfigFile path = do
    catch (runParseConfig path <$> BS.readFile path)
          (\(_ :: IOException) -> return defaultConfig)

runParseConfig :: String -> BS.ByteString -> VtyUserConfig
runParseConfig name cfgTxt =
  case runParser parseConfig () name cfgTxt of
    Right cfg -> cfg
    Left{}    -> defaultConfig

------------------------------------------------------------------------

type Parser = Parsec BS.ByteString ()

configLanguage :: Monad m => P.GenLanguageDef BS.ByteString () m
configLanguage = LanguageDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> oneOf "_'"
    , opStart         = opLetter configLanguage
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = []
    , reservedNames   = []
    , caseSensitive   = True
    }

configLexer :: Monad m => P.GenTokenParser BS.ByteString () m
configLexer = P.makeTokenParser configLanguage

mapDecl :: Parser VtyUserConfig
mapDecl = do
    "map" <- P.identifier configLexer
    termIdent <- (char '_' >> P.whiteSpace configLexer >> return Nothing)
             <|> (Just <$> P.stringLiteral configLexer)
    bytes     <- P.stringLiteral configLexer
    key       <- parseValue
    modifiers <- parseValue
    return defaultConfig { configInputMap = [(termIdent, bytes, EvKey key modifiers)] }

debugLogDecl :: Parser VtyUserConfig
debugLogDecl = do
    "debugLog" <- P.identifier configLexer
    path       <- P.stringLiteral configLexer
    return defaultConfig { configDebugLog = Just path }

widthMapDecl :: Parser VtyUserConfig
widthMapDecl = do
    "widthMap" <- P.identifier configLexer
    tName <- P.stringLiteral configLexer
    path <- P.stringLiteral configLexer
    return defaultConfig { configTermWidthMaps = [(tName, path)] }

ignoreLine :: Parser ()
ignoreLine = void $ manyTill anyChar newline

parseConfig :: Parser VtyUserConfig
parseConfig = liftM mconcat $ many $ do
    P.whiteSpace configLexer
    let directives = [try mapDecl, try debugLogDecl, try widthMapDecl]
    choice directives <|> (ignoreLine >> return defaultConfig)

class    Parse a        where parseValue :: Parser a
instance Parse Char     where parseValue = P.charLiteral configLexer
instance Parse Int      where parseValue = fromInteger <$> P.natural configLexer
instance Parse Key      where parseValue = genericParse
instance Parse Modifier where parseValue = genericParse
instance Parse a => Parse [a] where
  parseValue = P.brackets configLexer
                 (parseValue `sepBy` P.symbol configLexer ",")

------------------------------------------------------------------------
-- Derived parser for ADTs via generics
------------------------------------------------------------------------

genericParse :: (Generic a, GParse (Rep a)) => Parser a
genericParse = to <$> gparse

class    GParse f                      where gparse :: Parser (f a)
instance GParse f => GParse (M1 S i f) where gparse = M1 <$> gparse
instance GParse U1                     where gparse = return U1
instance Parse a => GParse (K1 i a)    where gparse = K1 <$> parseValue

instance (GParse f, GParse g) => GParse (f :*: g) where
  gparse = (:*:) <$> gparse <*> gparse

instance GParseAlts f => GParse (M1 D i f) where
  gparse =
    do con <- P.identifier configLexer
       M1 <$> gparseAlts con

------------------------------------------------------------------------

class GParseAlts f where
  gparseAlts :: String -> Parser (f a)

instance (Constructor i, GParse f) => GParseAlts (M1 C i f) where
  gparseAlts con =
    do guard (con == conName (M1 Nothing :: C1 i Maybe a))
       M1 <$> gparse

instance (GParseAlts f, GParseAlts g) => GParseAlts (f :+: g) where
  gparseAlts con = L1 <$> gparseAlts con <|> R1 <$> gparseAlts con

instance GParseAlts V1 where gparseAlts _ = fail "GParse: V1"

data ConfigUpdateResult =
    ConfigurationCreated
    | ConfigurationModified
    | ConfigurationConflict String
    | ConfigurationRedundant
    deriving (Eq, Show)

-- | Add a @widthMap@ directive to the Vty configuration file at the
-- specified path.
--
-- If the configuration path refers to a configuration that already
-- contains the directive for the specified map and terminal type, the
-- configuration file will not be modified. If the file does not contain
-- the directive, it will be appended to the file.
--
-- If the configuration path does not exist, a new configuration file
-- will be created and any directories in the path will also be created.
--
-- This returns a 'ConfigUpdateResult' indicating the change to the
-- configuration. This does not handle exceptions raised by file or
-- directory permissions issues.
addConfigWidthMap :: FilePath
                  -- ^ The configuration file path of the configuration
                  -- to modify or create.
                  -> String
                  -- ^ The @TERM@ value for the @widthMap@ directive.
                  -> FilePath
                  -- ^ The width table file path for the directive.
                  -> IO ConfigUpdateResult
addConfigWidthMap configPath term tablePath = do
    configEx <- doesFileExist configPath
    if configEx
       then updateConfig
       else createConfig >> return ConfigurationCreated

    where
        directive = "widthMap " <> show term <> " " <> show tablePath <> "\n"

        createConfig = do
            let dir = takeDirectory configPath
            createDirectoryIfMissing True dir
            writeFile configPath directive

        updateConfig = do
            config <- parseConfigFile configPath
            if (term, tablePath) `elem` configTermWidthMaps config
               then return ConfigurationRedundant
               else case lookup term (configTermWidthMaps config) of
                   Just other -> return $ ConfigurationConflict other
                   Nothing -> do
                       appendFile configPath directive
                       return ConfigurationModified
