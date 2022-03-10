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

import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)

-- | These sequences set xterm-based terminals to send focus event
-- sequences.
requestFocusEvents :: ByteString
requestFocusEvents = BS8.pack "\ESC[?1004h"

-- | These sequences disable focus events.
disableFocusEvents :: ByteString
disableFocusEvents = BS8.pack "\ESC[?1004l"

-- | Does the specified string begin with a focus event?
isFocusEvent :: ByteString -> Bool
isFocusEvent s = BS8.isPrefixOf focusIn s ||
                 BS8.isPrefixOf focusOut s

focusIn :: ByteString
focusIn = BS8.pack "\ESC[I"

focusOut :: ByteString
focusOut = BS8.pack "\ESC[O"

-- | Attempt to classify an input string as a focus event.
classifyFocusEvent :: ByteString -> KClass
classifyFocusEvent s = runParser s $ do
    when (not $ isFocusEvent s) failParse

    expectChar '\ESC'
    expectChar '['
    ty <- readChar
    case ty of
        'I' -> return EvGainedFocus
        'O' -> return EvLostFocus
        _   -> failParse
