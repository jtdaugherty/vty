{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Vty.Input.Interface where

import Graphics.Vty.Config
import Graphics.Vty.Input.Events

import Control.Concurrent.STM
import Control.Lens

import Data.IORef

import System.IO

data Input = Input
    { -- | Channel of events direct from input processing. Unlike 'nextEvent' this will not refresh
      -- the display if the next event is an 'EvResize'.
      _eventChannel  :: TChan Event
      -- | Shuts down the input processing. This should return the terminal input state to before
      -- the input initialized.
    , shutdownInput :: IO ()
      -- | Changes to this value are reflected after the next event.
    , _configRef :: IORef Config
      -- | input debug log
    , _inputDebug :: Maybe Handle
    }

makeLenses ''Input
