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

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Classify.Types

import Data.List (isPrefixOf, isInfixOf)

bracketedPasteStart :: String
bracketedPasteStart = "\ESC[200~"

bracketedPasteEnd :: String
bracketedPasteEnd = "\ESC[201~"

-- | Does the input start a bracketed paste?
bracketedPasteStarted :: String -> Bool
bracketedPasteStarted = isPrefixOf bracketedPasteStart

-- | Does the input contain a complete bracketed paste?
bracketedPasteFinished :: String -> Bool
bracketedPasteFinished = isInfixOf bracketedPasteEnd

-- | Parse a bracketed paste. This should only be called on a string if
-- both 'bracketedPasteStarted' and 'bracketedPasteFinished' return
-- 'True'.
parseBracketedPaste :: String -> KClass
parseBracketedPaste s =
    Valid (EvPaste p) (BS8.unpack $ BS8.drop (BS8.length end) rest')
    where
        start = BS8.pack bracketedPasteStart
        end   = BS8.pack bracketedPasteEnd
        (_, rest ) = BS8.breakSubstring start . BS8.pack $ s
        (p, rest') = BS8.breakSubstring end . BS8.drop (BS8.length start) $ rest
