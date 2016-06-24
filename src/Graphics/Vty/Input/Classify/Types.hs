-- | This module exports the input classification type to avoid import
-- cycles between other modules that need this.
module Graphics.Vty.Input.Classify.Types
    ( KClass(..)
    ) where

import Graphics.Vty.Input.Events

data KClass
    = Valid Event [Char]
    -- ^ A valid event was parsed. Any unused characters from the input
    -- stream are also provided.
    | Invalid
    -- ^ The input characters did not represent a valid event.
    | Prefix
    -- ^ The input characters form the prefix of a valid event character
    -- sequence.
    deriving(Show, Eq)
