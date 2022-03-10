-- | This module provides bracketed paste support as described at
--
-- http://cirw.in/blog/bracketed-paste
module Graphics.Vty.Input.Paste
  ( parseBracketedPaste
  , bracketedPasteStarted
  , bracketedPasteFinished
  )
where

import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Classify.Types

bracketedPasteStart :: ByteString
bracketedPasteStart = BS8.pack "\ESC[200~"

bracketedPasteEnd :: ByteString
bracketedPasteEnd = BS8.pack "\ESC[201~"

-- | Does the input start a bracketed paste?
bracketedPasteStarted :: ByteString -> Bool
bracketedPasteStarted = BS8.isPrefixOf bracketedPasteStart

-- | Does the input contain a complete bracketed paste?
bracketedPasteFinished :: ByteString -> Bool
bracketedPasteFinished = BS8.isInfixOf bracketedPasteEnd

-- | Parse a bracketed paste. This should only be called on a string if
-- both 'bracketedPasteStarted' and 'bracketedPasteFinished' return
-- 'True'.
parseBracketedPaste :: ByteString -> KClass
parseBracketedPaste s =
    Valid (EvPaste p) (BS8.drop endLen rest')
    where
        startLen = BS8.length bracketedPasteStart
        endLen   = BS8.length bracketedPasteEnd
        (_, rest ) = BS8.breakSubstring bracketedPasteStart s
        (p, rest') = BS8.breakSubstring bracketedPasteEnd . BS8.drop startLen $ rest
