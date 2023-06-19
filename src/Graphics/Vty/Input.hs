-- | This module provides the input abstraction for Vty.
module Graphics.Vty.Input
  ( Input(..)
  , module Graphics.Vty.Input.Events
  )
where

import Graphics.Vty.Input.Events
import Control.Concurrent.STM (TChan)

data Input =
    Input { eventChannel :: TChan InternalEvent
          -- ^ Channel of events direct from input processing. Unlike
          -- 'nextEvent' this will not refresh the display if the next
          -- event is an 'EvResize'.
          , shutdownInput :: IO ()
          -- ^ Shuts down the input processing. As part of shutting down
          -- the input, this should also restore the input state.
          , restoreInputState :: IO ()
          -- ^ Restore the terminal's input state to what it was prior
          -- to configuring input for Vty. This should be done as part
          -- of 'shutdownInput' but is exposed in case you need to
          -- access it directly.
          , inputLogMsg :: String -> IO ()
          -- ^ Log the specified message.
          }
