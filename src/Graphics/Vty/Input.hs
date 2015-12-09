{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

-- | The input layer for VTY. This provides methods for initializing an 'Input' structure which can
-- then be used to read 'Event's from the terminal.
--
-- There are two implementations:
--
--  1. Input from terminal devices. Using terminfo to aid event deserialization. Posix systems only.
--
--  2. Windows Console. Windows systems only.
--
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , Input(..)
                          , inputForConfig -- defined in the platform module
                          )
    where

import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Interface

#ifdef POSIX
import Graphics.Vty.Input.Posix
#endif

#ifdef WINDOWS
import Graphics.Vty.Input.Windows
#endif
