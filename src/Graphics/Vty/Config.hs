{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
-- | A 'Config' can be provided to mkVty to customize the applications use of vty. A config file can
-- be used to customize vty for a user's system.
--
-- The 'Config' provided is mappend'd to 'Config's loaded from @'getAppUserDataDirectory'/config@
-- and @$VTY_CONFIG_FILE@. The @$VTY_CONFIG_FILE@ takes precedence over the @config@ file or the
-- application provided 'Config'.
--
-- Lines in config files that fail to parse are ignored.  Later entries take precedence over
-- earlier.
--
-- For all directives:
-- 
-- @
--  string := \"\\\"\" chars+ \"\\\"\"
-- @
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
-- The value of the environment variable @VTY_DEBUG_LOG@ is equivalent to a debugLog entry at the
-- end of the last config file.
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
-- Set VTY_DEBUG_LOG. Run vty. Check debug log for incorrect mappings. Add corrected mappings to
-- .vty/config
--
module Graphics.Vty.Config where

#if __GLASGOW_HASKELL__ > 704
import Prelude
#else
import Prelude hiding (catch)
#endif

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
import System.Posix.Env (getEnv)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Types (Fd(..))

import Text.Parsec hiding ((<|>))
import Text.Parsec.Token ( GenLanguageDef(..) )
import qualified Text.Parsec.Token as P

-- | Mappings from input bytes to event in the order specified. Later entries take precedence over
-- earlier in the case multiple entries have the same byte string.
type InputMap = [(Maybe String, String, Event)]

data Config = Config
    {
    -- | The default is 1 character.
      vmin  :: Maybe Int
    -- | The default is 100 milliseconds, 0.1 seconds.
    , vtime :: Maybe Int
    -- | Debug information is appended to this file if not Nothing.
    , debugLog           :: Maybe FilePath
    -- | The (input byte, output event) pairs extend the internal input table of VTY and the table
    -- from terminfo.
    --
    -- See "Graphics.Vty.Config" module documentation for documentation of the @map@ directive.
    , inputMap           :: InputMap
    -- | The input file descriptor to use. The default is 'System.Posix.IO.stdInput'
    , inputFd           :: Maybe Fd
    -- | The output file descriptor to use. The default is 'System.Posix.IO.stdOutput'
    , outputFd          :: Maybe Fd
    -- | The terminal name used to look up terminfo capabilities.
    -- The default is the value of the TERM environment variable.
    , termName           :: Maybe String
    } deriving (Show, Eq)

instance Default Config where
    def = mempty

instance Monoid Config where
    mempty = Config
        { vmin         = Nothing
        , vtime        = Nothing
        , debugLog     = mempty
        , inputMap     = mempty
        , inputFd     = Nothing
        , outputFd    = Nothing
        , termName     = Nothing
        }
    mappend c0 c1 = Config
        -- latter config takes priority for everything but inputMap
        { vmin          = vmin c1     <|> vmin c0
        , vtime         = vtime c1    <|> vtime c0
        , debugLog      = debugLog c1 <|> debugLog c0
        , inputMap      = inputMap c0 <>  inputMap c1
        , inputFd      = inputFd c1 <|> inputFd c1
        , outputFd     = outputFd c1 <|> outputFd c1
        , termName      = termName c1 <|> termName c0
        }

type ConfigParser s a = ParsecT s () (Writer Config) a

-- | Config from @'getAppUserDataDirectory'/config@ and @$VTY_CONFIG_FILE@
userConfig :: IO Config
userConfig = do
    configFile <- (mappend <$> getAppUserDataDirectory "vty" <*> pure "/config") >>= parseConfigFile
    overrideConfig <- maybe (return def) parseConfigFile =<< getEnv "VTY_CONFIG_FILE"
    base <- (<> configFile <> overrideConfig) <$> standardIOConfig
    (mappend base) <$> overrideEnvConfig

overrideEnvConfig :: IO Config
overrideEnvConfig = do
    d <- getEnv "VTY_DEBUG_LOG"
    return $ def { debugLog = d }

standardIOConfig :: IO Config
standardIOConfig = do
    Just t <- getEnv "TERM"
    return $ def { vmin = Just 1
                 , vtime = Just 100
                 , inputFd = Just stdInput
                 , outputFd = Just stdOutput
                 , termName = Just t
                 }

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

mapDecl :: forall s u (m :: * -> *).
           (Monad m, Stream s (WriterT Config m) Char) =>
           ParsecT s u (WriterT Config m) ()
mapDecl = do
    void $ string "map"
    P.whiteSpace configLexer
    termIdent <- (char '_' >> P.whiteSpace configLexer >> return Nothing)
             <|> (Just <$> P.stringLiteral configLexer)
    bytes <- P.stringLiteral configLexer
    key <- parseKey
    modifiers <- parseModifiers
    lift $ tell $ def { inputMap = [(termIdent, bytes, EvKey key modifiers)] }

-- TODO: Generated by a vim macro. There is a better way here. Derive parser? Use Read
-- instance? Generics?
parseKey :: forall s u (m :: * -> *).
            Stream s m Char =>
            ParsecT s u m Key
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

parseModifiers :: forall s u (m :: * -> *).
                  Stream s m Char =>
                  ParsecT s u m [Modifier]
parseModifiers = P.brackets configLexer (parseModifier `sepBy` P.symbol configLexer ",")

parseModifier :: forall s u (m :: * -> *).
                 Stream s m Char =>
                 ParsecT s u m Modifier
parseModifier = do
    m <- P.identifier configLexer
    case m of
        "KMenu" -> return MShift
        "MCtrl" -> return MCtrl
        "MMeta" -> return MMeta
        "MAlt" -> return MAlt
        _ -> fail $ m ++ " is not a valid modifier identifier"

debugLogDecl :: forall s u (m :: * -> *).
                (Monad m, Stream s (WriterT Config m) Char) =>
                ParsecT s u (WriterT Config m) ()
debugLogDecl = do
    void $ string "debugLog"
    P.whiteSpace configLexer
    path <- P.stringLiteral configLexer
    lift $ tell $ def { debugLog = Just path }

ignoreLine :: forall s u (m :: * -> *).
              Stream s m Char => ParsecT s u m ()
ignoreLine = void $ manyTill anyChar newline

parseConfig :: forall s u (m :: * -> *).
               (Monad m, Stream s (WriterT Config m) Char) =>
               ParsecT s u (WriterT Config m) ()
parseConfig = void $ many $ do
    P.whiteSpace configLexer
    let directives = [mapDecl, debugLogDecl]
    try (choice directives) <|> ignoreLine
