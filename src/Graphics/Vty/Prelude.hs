-- | Prelude for Vty modules. Not particularly useful outside of Vty.
module Graphics.Vty.Prelude ( module Graphics.Vty.Prelude
                            , module Control.Applicative
                            , module Control.Monad
                            )
where

import Control.Applicative hiding ((<|>))
import Control.Monad

-- | Named alias for a Int pair
type DisplayRegion = (Int,Int)

regionWidth :: DisplayRegion -> Int
regionWidth = fst

regionHeight :: DisplayRegion -> Int
regionHeight = snd
