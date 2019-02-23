-- | Windows-based terminal output driver.
--
-- Copyright Jonathan Osser (osser97@gmail.com)
module Grapics.Vty.Output.Windows
  ( reserveTerminal
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr)
import Data.Terminfo.Parse
import Data.Terminfo.Eval

import Graphics.Vty.Attributes
import Graphics.Vty.Image (DisplayRegion)
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder (Write, writeToByteString, writeStorable)

import Control.Monad.Trans

import Data.Bits ((.&.))
import Data.Foldable (foldMap)
import Data.IORef
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Word

import Foreign.C.Types ( CInt(..), CLong(..) )
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)

import System.Win32


reserveTerminal :: ( Applicative m, MonadIO m ) => String -> HANDLE -> m Output
