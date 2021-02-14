module Graphics.Vty.Input.Focus
  ( requestFocusEvents
  , disableFocusEvents
  , isFocusEvent
  , classifyFocusEvent
  )
where

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Classify.Types
import Graphics.Vty.Input.Classify.Parse

import Control.Monad.State
import Data.List (isPrefixOf)

-- | These sequences set xterm-based terminals to send focus event
-- sequences.
requestFocusEvents :: String
requestFocusEvents = "\ESC[?1004h"

-- | These sequences disable focus events.
disableFocusEvents :: String
disableFocusEvents = "\ESC[?1004l"

-- | Does the specified string begin with a focus event?
isFocusEvent :: String -> Bool
isFocusEvent s = isPrefixOf focusIn s ||
                 isPrefixOf focusOut s

focusIn :: String
focusIn = "\ESC[I"

focusOut :: String
focusOut = "\ESC[O"

-- | Attempt to classify an input string as a focus event.
classifyFocusEvent :: String -> KClass
classifyFocusEvent s = runParser s $ do
    unless (isFocusEvent s) failParse

    expectChar '\ESC'
    expectChar '['
    ty <- readChar
    case ty of
        'I' -> return EvGainedFocus
        'O' -> return EvLostFocus
        _   -> failParse
