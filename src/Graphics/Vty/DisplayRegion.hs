-- Copyright 2009 Corey O'Connor
module Graphics.Vty.DisplayRegion
    where

import Data.Word

-- | Region of the terminal that vty will output to. Units are columns not characters.
data DisplayRegion = DisplayRegion 
    { region_width :: !Word
    , region_height :: !Word
    } deriving ( Show, Eq )

