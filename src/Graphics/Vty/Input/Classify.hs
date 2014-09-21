{-# OPTIONS_HADDOCK hide #-}
-- This makes a kind of tri. Has space efficiency issues with large input blocks.
-- Likely building a parser and just applying that would be better.
-- I did not write this so I might just rewrite it for better understanding. Which is not the best
-- of reasons.
-- TODO: measure and rewrite if required.
-- TODO: The ClassifyMap interface requires this code to always assure later entries override
-- earlier.
module Graphics.Vty.Input.Classify where

import Graphics.Vty.Input.Events

import Codec.Binary.UTF8.Generic (decode)

import Data.List(inits)
import qualified Data.Map as M( fromList, lookup )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S( fromList, member )

import Data.Char
import Data.Word

data KClass
    = Valid Event [Char]
    | Invalid
    | Prefix
    deriving(Show, Eq)

compile :: ClassifyMap -> [Char] -> KClass
compile table = cl' where
    -- take all prefixes and create a set of these
    prefixSet = S.fromList $ concatMap (init . inits . fst) $ table
    maxValidInputLength = maximum (map (length . fst) table)
    eventForInput = M.fromList table
    cl' [] = Prefix
    cl' inputBlock = case M.lookup inputBlock eventForInput of
            -- if the inputBlock is exactly what is expected for an event then consume the whole
            -- block and return the event
            Just e -> Valid e []
            Nothing -> case S.member inputBlock prefixSet of
                True -> Prefix
                -- look up progressively smaller tails of the input block until an event is found
                -- The assumption is that the event that consumes the most input bytes should be
                -- produced.
                -- The test verifyFullSynInputToEvent2x verifies this.
                -- H: There will always be one match. The prefixSet contains, by definition, all
                -- prefixes of an event. 
                False ->
                    let inputPrefixes = reverse $ take maxValidInputLength $ tail $ inits inputBlock
                    in case mapMaybe (\s -> (,) s `fmap` M.lookup s eventForInput) inputPrefixes of
                        (s,e) : _ -> Valid e (drop (length s) inputBlock)
                        -- neither a prefix or a full event.
                        -- TODO: debug log
                        [] -> Invalid

classify, classifyTab :: ClassifyMap -> [Char] -> KClass

-- As soon as
classify _table s@(c:_) | ord c >= 0xC2
    = if utf8Length (ord c) > length s then Prefix else classifyUtf8 s -- beginning of an utf8 sequence
classify table other
    = classifyTab table other

classifyUtf8 :: [Char] -> KClass
classifyUtf8 s = case decode ((map (fromIntegral . ord) s) :: [Word8]) of
    Just (unicodeChar, _) -> Valid (EvKey (KChar unicodeChar) []) []
    _ -> Invalid -- something bad happened; just ignore and continue.

classifyTab table = compile table

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

utf8Length :: (Num t, Ord a, Num a) => a -> t
utf8Length c
    | c < 0x80 = 1
    | c < 0xE0 = 2
    | c < 0xF0 = 3
    | otherwise = 4

