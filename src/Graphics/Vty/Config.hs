{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | A 'Config' can be provided to mkVty to customize the applications use of vty.
--
-- The 'Config' provided is mappend'd to 'Config's loaded from 'getAppUserDataDirectory'`/config`
-- and $VTY_CONFIG_FILE. The $VTY_CONFIG_FILE takes precedence over the `config` file or the
-- application provided 'Config'.
-- 
-- Each line of the input config is processed individually. Lines that fail to parse are ignored.
-- Later entries take precedence over earlier.
--
-- * Classify Table Overrides - "map"
--
-- Directive format:
--
-- @
--  entry := "map" string key modifier_list
--  key := KEsc | KChar Char | KBS ... (same as 'Key')
--  modifier_list := "[" modifier+ "]"
--  modifier := MShift | MCtrl | MMeta | MAlt
--  string := "\"" chars+ "\""
-- @
--
-- EG: If the contents of input.conf are
--
-- @
--  map "\ESC[B" KUp []
--  map "\ESC[1;3B" KDown [MAlt]
-- @
--
-- Then the bytes "\ESC[B" will result in the KUp event. The bytes "\ESC[1;3B" will result in the
-- event KDown with the MAlt modifier.
--
module Graphics.Vty.Config where

import Control.Applicative hiding (many)

import Control.Exception (catch, IOException)
import Control.Monad (void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import qualified Data.ByteString as BS
import Data.Default
import Data.Monoid

import Graphics.Vty.Input.Events

import System.Directory (getAppUserDataDirectory)
import System.Environment (lookupEnv)
import Text.Parsec hiding ((<|>))
import Text.Parsec.Token ( GenLanguageDef(..) )
import qualified Text.Parsec.Token as P

data Config = Config
    { specifiedEscPeriod :: Maybe Int            
    -- | Debug information about the input process is appended to the file.
    , debugInputLog      :: Maybe FilePath
    , inputOverrides     :: ClassifyTable
    } deriving (Show, Eq)

-- | AKA VTIME. The default is 100000 microseconds or 0.1 seconds. 
singleEscPeriod :: Config -> Int
singleEscPeriod = maybe 100000 id . specifiedEscPeriod

instance Default Config where
    def = Config
        { specifiedEscPeriod = Nothing
        , debugInputLog      = Nothing
        , inputOverrides     = []
        }

instance Monoid Config where
    mempty = def
    mappend c0 c1 = Config
        -- latter config takes priority in specifiedEscPeriod
        { specifiedEscPeriod = specifiedEscPeriod c1 <|> specifiedEscPeriod c0
        -- latter config takes priority in debugInputLog
        , debugInputLog = debugInputLog c1 <|> debugInputLog c0
        , inputOverrides = inputOverrides c0 <> inputOverrides c1
        }

type ConfigParser s a = ParsecT s () (Writer Config) a

userConfig :: IO Config
userConfig = do
    vtyConfig <- (mappend <$> getAppUserDataDirectory "vty" <*> pure "/config") >>= parseConfigFile
    overrideConfig <- lookupEnv "VTY_CONFIG_FILE" >>= maybe (return def) parseConfigFile
    return $ vtyConfig `mappend` overrideConfig

parseConfigFile :: FilePath -> IO Config
parseConfigFile path = do
    catch (runParseConfig path <$> BS.readFile path)
          (\(e :: IOException) -> return def)

runParseConfig :: Stream s (Writer Config) Char => String -> s -> Config
runParseConfig name = execWriter . runParserT parseConfig () name

-- I tried to use the haskellStyle here but that was specialized (without requirement?) to the
-- String stream type.
configLanguage :: Stream s m Char => P.GenLanguageDef s u m
configLanguage = LanguageDef
    { commentStart = "{-"
    , commentEnd = "-}"
    , commentLine = "--"
    , nestedComments = True
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> oneOf "_'"
    , opStart = opLetter configLanguage
    , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = []
    , reservedNames = []
    , caseSensitive = True
    }

configLexer :: Stream s m Char => P.GenTokenParser s u m
configLexer = P.makeTokenParser configLanguage

parseOverride = do
    void $ string "map"
    P.whiteSpace configLexer
    bytes <- P.stringLiteral configLexer
    key <- parseKey
    modifiers <- parseModifiers
    lift $ tell $ def { inputOverrides = [(bytes, EvKey key modifiers)] }

parseKey = do
    key <- P.identifier configLexer
    case key of
     "KChar" -> KChar <$> P.charLiteral configLexer
     "KFun" -> KFun . fromInteger <$> P.natural configLexer
     "KEsc" -> return KEsc
     "KBS" -> return KBS
     "KEnter" -> return KEnter
     "KLeft" -> return KLeft
     "KRight" -> return KRight
     "KUp" -> return KUp
     "KDown" -> return KDown
     "KUpLeft" -> return KUpLeft
     "KUpRight" -> return KUpRight
     "KDownLeft" -> return KDownLeft
     "KDownRight" -> return KDownRight
     "KCenter" -> return KCenter
     "KBackTab" -> return KBackTab
     "KPrtScr" -> return KPrtScr
     "KPause" -> return KPause
     "KIns" -> return KIns
     "KHome" -> return KHome
     "KPageUp" -> return KPageUp
     "KDel" -> return KDel
     "KEnd" -> return KEnd
     "KPageDown" -> return KPageDown
     "KBegin" -> return KBegin
     "KMenu" -> return KMenu
     _ -> fail $ key ++ " is not a valid key identifier"

parseModifiers = P.brackets configLexer (parseModifier `sepBy` P.symbol configLexer ",")

parseModifier = do
    m <- P.identifier configLexer
    case m of
        "KMenu" -> return MShift
        "MCtrl" -> return MCtrl
        "MMeta" -> return MMeta
        "MAlt" -> return MAlt
        _ -> fail $ m ++ " is not a valid modifier identifier"

ignoreLine = void $ manyTill anyChar newline

-- TODO: Generated by a vim macro. There is a better way here. Derive parser? Use Read
-- instance?
parseConfig = void $ many $ do
    P.whiteSpace configLexer
    let directives = [parseOverride]
    try (choice directives) <|> ignoreLine
