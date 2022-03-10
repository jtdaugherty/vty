-- | This module exports the input classification type to avoid import
-- cycles between other modules that need this.
{-# LANGUAGE StrictData #-}
module Graphics.Vty.Input.Classify.Types
  ( KClass(..)
  )
where

import Graphics.Vty.Input.Events

import Data.ByteString.Char8 (ByteString)

data KClass
    = Valid Event ByteString
    -- ^ A valid event was parsed. Any unused characters from the input
    -- stream are also provided.
    | Invalid
    -- ^ The input characters did not represent a valid event.
    | Prefix
    -- ^ The input characters form the prefix of a valid event character
    -- sequence.
    | Chunk
    -- ^ The input characters are either start of a bracketed paste chunk
    -- or in the middle of a bracketed paste chunk.
    deriving(Show, Eq)
