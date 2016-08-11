-- | This module provides bracketed paste support as described at
--
-- http://cirw.in/blog/bracketed-paste
module Graphics.Vty.Input.Paste
    ( parseBracketedPaste
    , bracketedPasteStarted
    , bracketedPasteFinished
    ) where

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
    let (p, rest) = takeUntil (drop (length bracketedPasteStart) s) bracketedPasteEnd
        rest' = if bracketedPasteEnd `isPrefixOf` rest
                then drop (length bracketedPasteEnd) rest
                else rest
    in Valid (EvPaste $ BS8.pack p) rest'

takeUntil :: (Eq a) => [a] -> [a] -> ([a],[a])
takeUntil [] _ = ([], [])
takeUntil cs sub
  | length cs < length sub      = (cs, [])
  | take (length sub) cs == sub = ([], drop (length sub) cs)
  | otherwise                   = let (pre, suf) = takeUntil (tail cs) sub
                                  in (head cs:pre, suf)
