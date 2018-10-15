{-# LANGUAGE CPP #-}

-- | Vty provides interfaces for both terminal input and terminal
-- output.
--
-- - Input to the terminal is provided to the Vty application as a
--   sequence of 'Event's.
--
-- - Output is provided to Vty by the application in the form of a
--   'Picture'. A 'Picture' is one or more layers of 'Image's.
--   'Image' values can be built by the various constructors in
--   "Graphics.Vty.Image". Output can be syled using 'Attr' (attribute)
--   values in the "Graphics.Vty.Attributes" module.
--
-- Vty uses threads internally, so programs made with Vty need to be
-- compiled with the threaded runtime using the GHC `-threaded` option.
--
-- @
--  import "Graphics.Vty"
--
--  main = do
--      cfg <- 'standardIOConfig'
--      vty <- 'mkVty' cfg
--      let line0 = 'string' ('defAttr' ` 'withForeColor' ` 'green') \"first line\"
--          line1 = 'string' ('defAttr' ` 'withBackColor' ` 'blue') \"second line\"
--          img = line0 '<->' line1
--          pic = 'picForImage' img
--      'update' vty pic
--      e <- 'nextEvent' vty
--      'shutdown' vty
--      'print' (\"Last event was: \" '++' 'show' e)
-- @
module Graphics.Vty
  ( Vty(..)
  , mkVty
  , Mode(..)
  , module Graphics.Vty.Config
  , module Graphics.Vty.Input
  , module Graphics.Vty.Output
  , module Graphics.Vty.Output.Interface
  , module Graphics.Vty.Picture
  , module Graphics.Vty.Image
  , module Graphics.Vty.Attributes
  )
where

import Graphics.Vty.Config
import Graphics.Vty.Input
import Graphics.Vty.Output
import Graphics.Vty.Output.Interface
import Graphics.Vty.Picture
import Graphics.Vty.Image
import Graphics.Vty.Attributes

import Control.Monad (when)
import Control.Concurrent.STM

import Data.IORef
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif

-- | A Vty value represents a handle to the Vty library that the
-- application must create in order to use Vty.
--
-- The use of Vty typically follows this process:
--
--    1. Initialize vty
--
--    2. Use 'update' to display a picture.
--
--    3. Use 'nextEvent' to get the next input event.
--
--    4. Depending on the event, go to 2 or 5.
--
--    5. Shutdown vty.
--
-- Operations on Vty handles are not thread-safe.
data Vty = Vty
    { -- | Outputs the given 'Picture'.
      update :: Picture -> IO ()
      -- | Return the next 'Event' or block until one becomes available.
    , nextEvent :: IO Event
      -- | Non-blocking version of 'nextEvent'.
    , nextEventNonblocking :: IO (Maybe Event)
      -- | The input interface. See 'Input'.
    , inputIface :: Input
      -- | The output interface. See 'Output'.
    , outputIface :: Output
      -- | Refresh the display. If other programs output to the terminal
      -- and mess up the display then the application might want to
      -- force a refresh using this function.
    , refresh :: IO ()
      -- | Clean up after vty. A call to this function is necessary to
      -- cleanly restore the terminal state before application exit. The
      -- above methods will throw an exception if executed after this is
      -- executed.
    , shutdown :: IO ()
    }

-- | Create a Vty handle. At most one handle should be created at a time
-- for a given terminal device.
--
-- The specified configuration is added to the the configuration
-- loaded by 'userConfig' with the 'userConfig' configuration taking
-- precedence. See "Graphics.Vty.Config".
--
-- For most applications @mkVty defaultConfig@ is sufficient.
mkVty :: Config -> IO Vty
mkVty appConfig = do
    config <- (<> appConfig) <$> userConfig
    input  <- inputForConfig config
    out    <- outputForConfig config
    intMkVty input out

intMkVty :: Input -> Output -> IO Vty
intMkVty input out = do
    reserveDisplay out

    shutdownVar <- atomically $ newTVar False
    let shutdownIo = do
            alreadyShutdown <- atomically $ swapTVar shutdownVar True
            when (not alreadyShutdown) $ do
                shutdownInput input
                releaseDisplay out
                releaseTerminal out

    lastPicRef <- newIORef Nothing
    lastUpdateRef <- newIORef Nothing

    let innerUpdate inPic = do
            b <- displayBounds out
            mlastUpdate <- readIORef lastUpdateRef
            updateData <- case mlastUpdate of
                Nothing -> do
                    dc <- displayContext out b
                    outputPicture dc inPic
                    return (b, dc)
                Just (lastBounds, lastContext) -> do
                    if b /= lastBounds
                        then do
                            dc <- displayContext out b
                            outputPicture dc inPic
                            return (b, dc)
                        else do
                            outputPicture lastContext inPic
                            return (b, lastContext)
            writeIORef lastUpdateRef $ Just updateData
            writeIORef lastPicRef $ Just inPic

    let innerRefresh = do
            writeIORef lastUpdateRef Nothing
            bounds <- displayBounds out
            dc <- displayContext out bounds
            writeIORef (assumedStateRef $ contextDevice dc) initialAssumedState
            mPic <- readIORef lastPicRef
            maybe (return ()) innerUpdate mPic

    let mkResize = uncurry EvResize <$> displayBounds out
        gkey = do
            k <- atomically $ readTChan $ _eventChannel input
            case k of
                (EvResize _ _)  -> mkResize
                _ -> return k
        gkey' = do
            k <- atomically $ tryReadTChan $ _eventChannel input
            case k of
                (Just (EvResize _ _))  -> Just <$> mkResize
                _ -> return k

    return $ Vty { update = innerUpdate
                 , nextEvent = gkey
                 , nextEventNonblocking = gkey'
                 , inputIface = input
                 , outputIface = out
                 , refresh = innerRefresh
                 , shutdown = shutdownIo
                 }
