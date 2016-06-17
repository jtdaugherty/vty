module Graphics.Vty.Input.Classify.Types
    ( KClass(..)
    ) where

import Graphics.Vty.Input.Events

data KClass
    = Valid Event [Char]
    | Invalid
    | Prefix
    deriving(Show, Eq)
