-- Copyright 2009 Corey O'Connor
module Graphics.Vty.DisplayRegion
    where

import Data.Word

data DisplayRegion = DisplayRegion 
    { region_width :: !Word 
    , region_height :: !Word
    } deriving ( Show, Eq )

