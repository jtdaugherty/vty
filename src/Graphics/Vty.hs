-- | Vty supports input and output to terminal devices.
--
--  - The input is provides as a sequence of 'Event's.
--
--  - The output is defined by a 'Picture'. Which is one or more layers of 'Image's.
--      The constructors in "Graphics.Vty.Image.Internal" should not be used directly. The module
--      "Graphics.Vty.Image" provides a number of constructor equations that will build correct
--      'Image' values. See 'string', '<|>', and '<->' for starters.
--
--  - 'Image's can be styled using 'Attr'. See "Graphics.Vty.Attributes".
-- 
-- Good sources of documentation for terminal programming are:
--
--  - <https://github.com/b4winckler/vim/blob/master/src/term.c>
--
--  - <http://invisible-island.net/xterm/ctlseqs/ctlseqs.html>
--
--  - <http://ulisse.elettra.trieste.it/services/doc/serial/config.html>
--
--  - <http://www.leonerd.org.uk/hacks/hints/xterm-8bit.html>
--
--  - <http://www.unixwiz.net/techtips/termios-vmin-vtime.html>
--
--  - <http://vt100.net/docs/vt100-ug/chapter3.html vt100 control sequences>
module Graphics.Vty ( Vty(..)
                    , mkVty
                    , mkVtyEscDelay
                    , module Graphics.Vty.Output
                    , module Graphics.Vty.Picture
                    , DisplayRegion
                    , Key(..)
                    , Modifier(..)
                    , Button(..)
                    , Event(..)
                    ) 
    where

import Graphics.Vty.Prelude

import Graphics.Vty.Input
import Graphics.Vty.Output
import Graphics.Vty.Picture

import Data.IORef

import qualified System.Console.Terminfo as Terminfo

-- | The main object.  At most one should be created.
--
-- The use of Vty typically follows this process:
--
--    0. initialize vty
--
--    1. use the update equation of Vty to display a picture
--
--    2. repeat
--
--    3. shutdown vty.
--
-- An alternative to tracking the Vty instance is to use 'withVty' in "Graphics.Vty.Inline.Unsafe".
--
-- This does not assure any thread safety. In theory, as long as an update action is not executed
-- when another update action is already then it's safe to call this on multiple threads.
--
-- \todo Remove explicit `shutdown` requirement.
data Vty = Vty 
    { -- | Outputs the given Picture. Equivalent to output_picture applied to a display context
      -- implicitly managed by Vty.  
      update :: Picture -> IO ()
      -- | Get one Event object, blocking if necessary.
    , next_event :: IO Event
      -- | The output interface. See `Output`
    , output_iface :: Output
      -- | Refresh the display. Normally the library takes care of refreshing.  Nonetheless, some
      -- other program might output to the terminal and mess the display.  In that case the user
      -- might want to force a refresh.
    , refresh :: IO ()
      -- | Clean up after vty.
      -- The above methods will throw an exception if executed after this is executed.
    , shutdown :: IO () 
    }

-- | Set up the state object for using vty.  At most one state object should be
-- created at a time.
mkVty :: IO Vty
mkVty = mkVtyEscDelay defaultEscDelay

-- | Set up the state object for using vty.  At most one state object should be
-- created at a time. The delay, in microseconds, specifies the period of time to wait for a key
-- following reading ESC from the terminal before considering the ESC key press as a discrete event.
-- \todo move input init into terminal interface
mkVtyEscDelay :: Int -> IO Vty
mkVtyEscDelay escDelay = do
    term_info <- Terminfo.setupTermFromEnv
    t <- output_for_current_terminal
    reserve_display t
    (kvar, endi) <- initTermInput escDelay term_info
    intMkVty kvar ( endi >> release_display t >> release_terminal t ) t

intMkVty :: IO Event -> IO () -> Output -> IO Vty
intMkVty kvar fend out = do
    last_pic_ref <- newIORef Nothing
    last_update_ref <- newIORef Nothing

    let inner_update in_pic = do
            b@(w,h) <- display_bounds out
            let cursor  = pic_cursor in_pic
                in_pic' = case cursor of
                  Cursor x y ->
                    let
                       x'      = case x of
                                   _ | x < 0     -> 0
                                     | x >= w    -> w - 1
                                     | otherwise -> x
                       y'      = case y of
                                   _ | y < 0     -> 0
                                     | y >= h    -> h - 1
                                     | otherwise -> y
                     in in_pic { pic_cursor = Cursor x' y' }
                  _          -> in_pic
            mlast_update <- readIORef last_update_ref
            update_data <- case mlast_update of
                Nothing -> do
                    dc <- display_context out b
                    output_picture dc in_pic'
                    return (b, dc)
                Just (last_bounds, last_context) -> do
                    if b /= last_bounds
                        then do
                            dc <- display_context out b
                            output_picture dc in_pic'
                            return (b, dc)
                        else do
                            output_picture last_context in_pic'
                            return (b, last_context)
            writeIORef last_update_ref $ Just update_data
            writeIORef last_pic_ref $ Just in_pic'

    let inner_refresh 
            =   writeIORef last_update_ref Nothing
            >>  readIORef last_pic_ref 
            >>= maybe ( return () ) ( \pic -> inner_update pic ) 

    let gkey = do k <- kvar
                  case k of 
                    (EvResize _ _)  -> inner_refresh
                                       >> display_bounds out
                                       >>= return . (\(w,h)-> EvResize w h)
                    _               -> return k

    return $ Vty { update = inner_update
                 , next_event = gkey
                 , output_iface = out
                 , refresh = inner_refresh
                 , shutdown = fend 
                 }

