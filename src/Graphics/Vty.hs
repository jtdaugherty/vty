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
                    , module Graphics.Vty.Input
                    , module Graphics.Vty.Output
                    , module Graphics.Vty.Picture
                    , DisplayRegion
                    ) 
    where

import Graphics.Vty.Prelude

import Graphics.Vty.Input
import Graphics.Vty.Output
import Graphics.Vty.Picture

import Control.Concurrent

import Data.IORef

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
    { -- | Outputs the given Picture. Equivalent to 'outputPicture' applied to a display context
      -- implicitly managed by Vty. The managed display context is reset on resize.
      update :: Picture -> IO ()
      -- | Get one Event object, blocking if necessary. This will refresh the terminal if the event
      -- is a 'EvResize'.
    , nextEvent :: IO Event
      -- | The input interface. See 'Input'
    , inputIface :: Input
      -- | The output interface. See 'Output'
    , outputIface :: Output
      -- | Refresh the display. Normally the library takes care of refreshing.  Nonetheless, some
      -- other program might output to the terminal and mess up the display.  In that case the
      -- application might want to force a refresh.
    , refresh :: IO ()
      -- | Clean up after vty.
      -- The above methods will throw an exception if executed after this is executed.
    , shutdown :: IO () 
    }

-- | Set up the state object for using vty.  At most one state object should be
-- created at a time.
mkVty :: Config -> IO Vty
mkVty config = do
    input <- inputForCurrentTerminal config
    out <- outputForCurrentTerminal
    intMkVty input out

intMkVty :: Input -> Output -> IO Vty
intMkVty input out = do
    reserveDisplay out
    let shutdownIo = do
            shutdownInput input
            releaseDisplay out
            releaseTerminal out
    lastPicRef <- newIORef Nothing
    lastUpdateRef <- newIORef Nothing

    let innerUpdate inPic = do
            b@(w,h) <- displayBounds out
            let cursor  = picCursor inPic
                inPic' = case cursor of
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
                     in inPic { picCursor = Cursor x' y' }
                  _ -> inPic
            mlastUpdate <- readIORef lastUpdateRef
            updateData <- case mlastUpdate of
                Nothing -> do
                    dc <- displayContext out b
                    outputPicture dc inPic'
                    return (b, dc)
                Just (lastBounds, lastContext) -> do
                    if b /= lastBounds
                        then do
                            dc <- displayContext out b
                            outputPicture dc inPic'
                            return (b, dc)
                        else do
                            outputPicture lastContext inPic'
                            return (b, lastContext)
            writeIORef lastUpdateRef $ Just updateData
            writeIORef lastPicRef $ Just inPic'

    let innerRefresh 
            =   writeIORef lastUpdateRef Nothing
            >>  readIORef lastPicRef 
            >>= maybe ( return () ) ( \pic -> innerUpdate pic ) 

    let gkey = do k <- readChan $ _eventChannel input
                  case k of 
                    (EvResize _ _)  -> innerRefresh
                                       >> displayBounds out
                                       >>= return . (\(w,h)-> EvResize w h)
                    _               -> return k

    return $ Vty { update = innerUpdate
                 , nextEvent = gkey
                 , inputIface = input
                 , outputIface = out
                 , refresh = innerRefresh
                 , shutdown = shutdownIo
                 }

