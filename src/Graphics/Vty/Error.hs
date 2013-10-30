module Graphics.Vty.Error
    where

import Control.Exception

-- | The type of exceptions specific to vty.
--
-- These have fully qualified names by default since, IMO, exception handling requires this.
data VtyException
    =  VtyFailure String -- ^ Uncategorized failure specific to vty.
