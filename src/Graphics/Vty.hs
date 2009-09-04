-- Copyright 2009 Corey O'Connor
{-# LANGUAGE ForeignFunctionInterface, BangPatterns, UnboxedTuples #-}
{-# CFILES gwinsz.c #-}

-- Good sources of documentation for terminal programming are:
-- vt100 control sequences: http://vt100.net/docs/vt100-ug/chapter3.html#S3.3.3
-- Xterm control sequences: http://invisible-island.net/xterm/ctlseqs/ctlseqs.html

module Graphics.Vty ( Vty(..)
                    , mkVty
                    , mkVtyEscDelay
                    , module Graphics.Vty.Terminal
                    , module Graphics.Vty.Picture
                    , module Graphics.Vty.DisplayRegion
                    , Key(..)
                    , Modifier(..)
                    , Button(..)
                    , Event(..)
                    ) 
    where


import Graphics.Vty.Terminal
import Graphics.Vty.Picture
import Graphics.Vty.DisplayRegion
import Graphics.Vty.LLInput

import Data.IORef
import Control.Concurrent

import Data.Maybe ( maybe )

import System.Console.Terminfo
import System.IO

-- | The main object.  At most one should be created.
-- An alternative is to use unsafePerformIO to automatically create a singleton Vty instance when
-- required.
--
-- This has the same thread safety as `output_picture`.
-- 
-- todo: Once the Terminal interface encompasses input this interface will be deprecated.
-- Currently, just using the Terminal interface there is no support for input events.
data Vty = Vty 
    { -- | Outputs the given Picture. Equivalent to output_picture. 
      update :: Picture -> IO ()
      -- | Get one Event object, blocking if necessary.
    , next_event :: IO Event
      -- | Handle to the terminal interface. See `Terminal`
      -- 
      -- todo: provide a similar abstraction for input. Use haskeline's input backend for
      -- implementation.
      --
      -- The use of Vty typically follows this process:
      --
      --    0. initialize vty
      --
      --    1. use the update equation of Vty to display a picture
      --
      --    2. repeat
      --
      --    3. shutdown vty. todo: remove? Automate release of resources as much as possible.
      -- 
      -- This version currently supports the same interface.
    , terminal :: TerminalHandle
      -- | Refresh the display. Normally the library takes care of refreshing. 
      -- Nonetheless, some other program might output to the terminal and mess the display.
      -- In that case the user might want to force a refresh.
    , refresh :: IO ()
      -- | Clean up after vty.
    , shutdown :: IO () 
    }

-- | Set up the state object for using vty.  At most one state object should be
-- created at a time.
mkVty :: IO Vty
mkVty = mkVtyEscDelay 0

mkVtyEscDelay :: Int -> IO Vty
mkVtyEscDelay escDelay = do 
    term_info <- setupTermFromEnv 
    t <- terminal_handle
    reserve_display t
    (kvar, endi) <- initTermInput escDelay term_info
    intMkVty kvar ( endi >> release_display t >> release_terminal t ) t

intMkVty :: IO Event -> IO () -> TerminalHandle -> IO Vty
intMkVty kvar fend t = do
    last_pic_ref <- newIORef undefined
    last_update_ref <- newIORef Nothing

    let inner_update in_pic = do
            b <- display_bounds t
            mlast_update <- readIORef last_update_ref
            update_data <- case mlast_update of
                Nothing -> do
                    d <- display_context t b
                    output_picture d in_pic
                    return (b, d)
                Just (last_bounds, last_context) -> do
                    if b /= last_bounds
                        then do
                            d <- display_context t b
                            output_picture d in_pic
                            return (b, d)
                        else do
                            output_picture last_context in_pic
                            return (b, last_context)
            writeIORef last_update_ref $ Just update_data
            writeIORef last_pic_ref $ Just in_pic

    let inner_refresh 
            = readIORef last_pic_ref 
            >>= maybe ( return () ) ( \pic -> inner_update pic ) 

    let gkey = do k <- kvar
                  case k of 
                    (EvResize _ _)  -> inner_refresh 
                                       >> display_bounds t 
                                       >>= return . ( \(DisplayRegion w h) 
                                                        -> EvResize (fromEnum w) (fromEnum h)
                                                    )
                    _               -> return k

    return $ Vty { update = inner_update
                 , next_event = gkey
                 , terminal = t
                 , refresh = inner_refresh
                 , shutdown = fend 
                 }

