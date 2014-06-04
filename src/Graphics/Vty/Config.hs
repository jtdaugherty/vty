{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | A 'Config' can be provided to mkVty to customize the applications use of vty. A config file can
-- be used to customize vty for a user's system.
--
-- The 'Config' provided is mappend'd to 'Config's loaded from @'getAppUserDataDirectory'/config@
-- and @$VTY_CONFIG_FILE@. The @$VTY_CONFIG_FILE@ takes precedence over the @config@ file or the
-- application provided 'Config'.
--
-- Each line of the input config is processed individually. Lines that fail to parse are ignored.
-- Later entries take precedence over earlier.
--
-- For all directives:
-- 
-- @
--  string := \"\\\"\" chars+ \"\\\"\"
-- @
--
-- = Debug Directives
--
-- == @debugLog@
--
-- Format:
--
-- @
--  \"debugLog\" string
-- @
--
-- The value of the environment variable @VTY_DEBUG_LOG@ is equivalent to a debugLog entry at the
-- end of the last config file.
--
-- = Input Table Directives
--
-- == @map@
--
-- Directive format:
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
-- EG: If the contents are
--
-- @
--  map _       \"\\ESC[B\"    KUp   []
--  map _       \"\\ESC[1;3B\" KDown [MAlt]
--  map "xterm" \"\\ESC[D\"    KLeft []
-- @
--
-- Then the bytes @\"\\ESC[B\"@ will result in the KUp event on all terminals. The bytes
-- @\"\\ESC[1;3B\"@ will result in the event KDown with the MAlt modifier on all terminals.
-- The bytes @\"\\ESC[D\"@ will result in the KLeft event when @TERM@ is @xterm@.
--
-- If a debug log is requested then vty will output the current input table to the log in the above
-- format.
--
-- EG: Set VTY_DEBUG_LOG. Run vty. Check debug log for incorrect mappings. Add corrected mappings to
-- .vty/config
--
module Graphics.Vty.Config where

-- ignore warning on GHC 7.6+. Required for GHC 7.4
import Prelude hiding (catch)

import Control.Applicative hiding (many)

import Control.Exception (tryJust, catch, IOException)
import Control.Monad (void, guard)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import qualified Data.ByteString as BS
import Data.Default
import Data.Monoid

import Graphics.Vty.Input.Events

import System.Directory (getAppUserDataDirectory)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

import Text.Parsec hiding ((<|>))
import Text.Parsec.Token ( GenLanguageDef(..) )
import qualified Text.Parsec.Token as P

-- | Mappings from input bytes to event in the order specified. Later entries take precedence over
-- earlier in the case multiple entries have the same byte string.
type InputMap = [(Maybe String, String, Event)]

data Config = Config
    { specifiedEscPeriod :: Maybe Int            
    -- | Debug information is appended to this file if not Nothing.
    , debugLog           :: Maybe FilePath
    -- | The (input byte, output event) pairs extend the internal input table of VTY and the table
    -- from terminfo.
    --
    -- See "Graphics.Vty.Config" module documentation for documentation of the @map@ directive.
    , inputMap           :: InputMap
    } deriving (Show, Eq)

-- | AKA VTIME. The default is 100000 microseconds or 0.1 seconds. Set using the
-- 'specifiedEscPeriod' field of 'Config'
singleEscPeriod :: Config -> Int
singleEscPeriod = maybe 100000 id . specifiedEscPeriod

instance Default Config where
    def = Config
        { specifiedEscPeriod = Nothing
        , debugLog           = Nothing
        , inputOverrides     = []
        }

instance Monoid Config where
    mempty = def
    mappend c0 c1 = Config
        -- latter config takes priority in specifiedEscPeriod
        { specifiedEscPeriod = specifiedEscPeriod c1 <|> specifiedEscPeriod c0
        -- latter config takes priority in debugInputLog
        , debugLog = debugLog c1 <|> debugLog c0
        , inputOverrides = inputOverrides c0 <> inputOverrides c1
        }

type ConfigParser s a = ParsecT s () (Writer Config) a

-- | Config from @'getAppUserDataDirectory'/config@ and @$VTY_CONFIG_FILE@
userConfig :: IO Config
userConfig = do
    userConfig <- (mappend <$> getAppUserDataDirectory "vty" <*> pure "/config") >>= parseConfigFile
    overridePath <- tryJust (guard . isDoesNotExistError) $ getEnv "VTY_CONFIG_FILE"
    overrideConfig <- either (const $ return def) parseConfigFile overridePath
    debugLogPath <- tryJust (guard . isDoesNotExistError) $ getEnv "VTY_DEBUG_LOG"
    let debugLogConfig = either (const def) (\p -> def { debugLog = Just p }) debugLogPath
    return $ mconcat [userConfig, overrideConfig, debugLogConfig]

parseConfigFile :: FilePath -> IO Config
parseConfigFile path = do
    catch (runParseConfig path <$> BS.readFile path)
          (\(_ :: IOException) -> return def)

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

mapDecl = do
    void $ string "map"
    P.whiteSpace configLexer
    termIdent <- (char "_" >> return Nothing) <|> (Just <$> P.stringLiteral configLexer)
    bytes <- P.stringLiteral configLexer
    key <- parseKey
    modifiers <- parseModifiers
    lift $ tell $ def { inputOverrides = [(termIdent, bytes, EvKey key modifiers)] }

-- TODO: Generated by a vim macro. There is a better way here. Derive parser? Use Read
-- instance?
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

debugLogDecl = do
    void $ string "debugLog"
    P.whiteSpace configLexer
    path <- P.stringLiteral configLexer
    lift $ tell $ def { debugLog = Just path }

ignoreLine = void $ manyTill anyChar newline

parseConfig = void $ many $ do
    P.whiteSpace configLexer
    let directives = [mapDecl, debugLogDecl]
    try (choice directives) <|> ignoreLine
