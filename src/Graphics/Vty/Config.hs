-- | A 'Config' can be provided to mkVty to customize the applications use of vty.
--
-- Parts of the structure can be overriden by $HOME/.config/vty.config and then $VTY_CONFIG_FILE.
--
-- See also 'classifyTableUserOverrides'.
--
module Graphics.Vty.Config where

import Data.Default
import Data.Either (either)
import Data.Monoid

import Graphics.Vty.Input.Events

import System.FilePath

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data Config = Config
    { singleEscPeriod   :: Int            -- ^ AKA VTIME. The default is 100000 microseconds or 0.1 seconds.
    -- | Debug information about the input process is appended to the file.
    , debugInputLog     :: Maybe FilePath
    , inputOverrides    :: ClassifyTable
    } deriving (Show, Eq)

instance Default Config where
    def = Config
        { singleEscPeriod = 100000
        , debugInputLog = Nothing
        , inputOverrides = []
        }

-- not a proper monoid but useful as one.
instance Monoid Config where
    mempty = def
    mappend c0 c1 = c1 { inputOverrides = inputOverrides c0 <> inputOverrides c1 }

parseConfigFile :: FilePath -> IO Config
parseConfigFile path = either (const def) id <$> parseFromFile parseConfig path

parseConfig = do
    let lexer = P.makeTokenParser haskellDef
        parseOverride = do
            P.whiteSpace lexer
            string "map"
            P.whiteSpace lexer
            bytes <- P.stringLiteral
            key <- parseKey
            modifier <- parseModifier
            _ <- manyTill space newline
    overrides <- many $ try parseOverride <|> ignoreOverride
