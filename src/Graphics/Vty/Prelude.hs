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

region_width :: DisplayRegion -> Int
region_width = fst

region_height :: DisplayRegion -> Int
region_height = snd
