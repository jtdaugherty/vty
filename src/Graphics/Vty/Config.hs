-- | A 'Config' can be provided to mkVty to customize the applications use of vty.
--
-- Parts of the structure can be overriden by $HOME/.config/vty.config and then $VTY_CONFIG_FILE.
--
-- See also 'classifyTableUserOverrides'.
--
module Graphics.Vty.Config where

import Data.Default

import System.FilePath

data Config = Config
    { singleEscPeriod   :: Int            -- ^ AKA VTIME. The default is 100000 microseconds or 0.1 seconds.
    -- | Debug information about the input process is appended to the file.
    , debugInputLog     :: Maybe FilePath
    } deriving (Show, Eq)

instance Default Config where
    def = Config
        { singleEscPeriod = 100000
        , debugInputLog = Nothing
        }

