{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- | This module provides an abstract interface for performing terminal
-- output. The only user-facing part of this API is 'Output'.
module Graphics.Vty.Input.Interface where

import Graphics.Vty.Config
import Graphics.Vty.Input.Classify
import Graphics.Vty.Input.Events

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (mask, try, SomeException)
import Lens.Micro hiding ((<>~))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Control.Monad (when, mzero, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.Trans.Reader (ReaderT(..))

import Data.Char
import Data.IORef
import Data.Word (Word8)
  
import System.IO

  
data Input = Input
    { -- | Channel of events direct from input processing. Unlike
      -- 'nextEvent' this will not refresh the display if the next event
      -- is an 'EvResize'.
      _eventChannel  :: TChan Event
      -- | Shuts down the input processing. This should return the
      -- terminal input state to before he input initialized.
    , shutdownInput :: IO ()
      -- | Changes to this value are reflected after the next event.
    , _configRef :: IORef Config
      -- | input debug log
    , _inputDebug :: Maybe Handle
    }

makeLenses ''Input
