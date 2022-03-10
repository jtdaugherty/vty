{-# OPTIONS_HADDOCK hide #-}
-- This makes a kind of trie. Has space efficiency issues with large
-- input blocks. Likely building a parser and just applying that would
-- be better.
module Graphics.Vty.Input.Classify
  ( classify
  , KClass(..)
  , ClassifierState(..)
  )
where

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Mouse
import Graphics.Vty.Input.Focus
import Graphics.Vty.Input.Paste
import Graphics.Vty.Input.Classify.Types

import Codec.Binary.UTF8.Generic (decode)

import Control.Arrow (first)
import qualified Data.Map as M( fromList, lookup )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S( fromList, member )

import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)

-- | Whether the classifier is currently processing a chunked format.
-- Currently, only bracketed pastes use this.
data ClassifierState
    = ClassifierStart
    -- ^ Not processing a chunked format.
    | ClassifierInChunk ByteString [ByteString]
    -- ^ Currently processing a chunked format. The initial chunk is in the
    -- first argument and a reversed remainder of the chunks is collected in
    -- the second argument. At the end of the processing, the chunks are
    -- reversed and concatenated with the final chunk.

compile :: ClassifyMap -> ByteString -> KClass
compile table = cl' where
    -- take all prefixes and create a set of these
    prefixSet = S.fromList $ concatMap (init . BS.inits . BS8.pack . fst) table
    maxValidInputLength = maximum (map (length . fst) table)
    eventForInput = M.fromList $ map (first BS8.pack) table
    cl' inputBlock | BS8.null inputBlock = Prefix
    cl' inputBlock = case M.lookup inputBlock eventForInput of
            -- if the inputBlock is exactly what is expected for an
            -- event then consume the whole block and return the event
            Just e -> Valid e BS8.empty
            Nothing -> case S.member inputBlock prefixSet of
                True -> Prefix
                -- look up progressively smaller tails of the input
                -- block until an event is found The assumption is that
                -- the event that consumes the most input bytes should
                -- be produced.
                -- The test verifyFullSynInputToEvent2x verifies this.
                -- H: There will always be one match. The prefixSet
                -- contains, by definition, all prefixes of an event.
                False ->
                    let inputPrefixes = reverse . take maxValidInputLength . tail . BS8.inits $ inputBlock
                    in case mapMaybe (\s -> (,) s `fmap` M.lookup s eventForInput) inputPrefixes of
                        (s,e) : _ -> Valid e (BS8.drop (BS8.length s) inputBlock)
                        -- neither a prefix or a full event.
                        [] -> Invalid

classify :: ClassifyMap -> ClassifierState -> ByteString -> KClass
classify table = process
    where
        standardClassifier = compile table

        process ClassifierStart s =
            case BS.uncons s of
                _ | bracketedPasteStarted s ->
                    if bracketedPasteFinished s
                    then parseBracketedPaste s
                    else Chunk
                _ | isMouseEvent s      -> classifyMouseEvent s
                _ | isFocusEvent s      -> classifyFocusEvent s
                Just (c,cs) | c >= 0xC2 -> classifyUtf8 c cs
                _                       -> standardClassifier s

        process (ClassifierInChunk p ps) s | bracketedPasteStarted p =
            if bracketedPasteFinished s
            then parseBracketedPaste $ BS.concat $ p:reverse (s:ps)
            else Chunk
        process ClassifierInChunk{} _ = Invalid

classifyUtf8 :: Word8 -> ByteString -> KClass
classifyUtf8 c cs =
  let n = utf8Length c
      (codepoint,rest) = BS8.splitAt (n - 1) cs

      codepoint8 :: [Word8]
      codepoint8 = c:BS.unpack codepoint

  in case decode codepoint8 of
       _ | n < BS.length codepoint + 1 -> Prefix
       Just (unicodeChar, _)           -> Valid (EvKey (KChar unicodeChar) []) rest
       -- something bad happened; just ignore and continue.
       Nothing                         -> Invalid

utf8Length :: Word8 -> Int
utf8Length c
    | c < 0x80 = 1
    | c < 0xE0 = 2
    | c < 0xF0 = 3
    | otherwise = 4
