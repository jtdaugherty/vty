-- | This module provides a simple parser for parsing input event
-- control sequences.
module Graphics.Vty.Input.Classify.Parse
  ( Parser
  , runParser
  , failParse
  , readInt
  , readChar
  , expectChar
  )
where

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Classify.Types

import Control.Monad.Trans.Maybe
import Control.Monad.State

import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)

type Parser a = MaybeT (State ByteString) a

-- | Run a parser on a given input string. If the parser fails, return
-- 'Invalid'. Otherwise return the valid event ('Valid') and the
-- remaining unparsed characters.
runParser :: ByteString -> Parser Event -> KClass
runParser s parser =
    case runState (runMaybeT parser) s of
        (Nothing, _)        -> Invalid
        (Just e, remaining) -> Valid e remaining

-- | Fail a parsing operation.
failParse :: Parser a
failParse = fail "invalid parse"

-- | Read an integer from the input stream. If an integer cannot be
-- read, fail parsing. E.g. calling readInt on an input of "123abc" will
-- return '123' and consume those characters.
readInt :: Parser Int
readInt = do
    s <- BS8.unpack <$> get
    case (reads :: ReadS Int) s of
        [(i, rest)] -> put (BS8.pack rest) >> return i
        _ -> failParse

-- | Read a character from the input stream. If one cannot be read (e.g.
-- we are out of characters), fail parsing.
readChar :: Parser Char
readChar = do
    s <- get
    case BS8.uncons s of
        Just (c,rest) -> put rest >> return c
        Nothing -> failParse

-- | Read a character from the input stream and fail parsing if it is
-- not the specified character.
expectChar :: Char -> Parser ()
expectChar c = do
    c' <- readChar
    if c' == c then return () else failParse
