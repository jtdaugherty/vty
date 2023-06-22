{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Vty provides interfaces for both terminal input and terminal
-- output.
--
-- - User input to the terminal is provided to the Vty application as a
--   sequence of 'Event's.
--
-- - Output is provided to by the application to Vty in the form of a
--   'Picture'. A 'Picture' is one or more layers of 'Image's.
--   'Image' values can be built by the various constructors in
--   "Graphics.Vty.Image". Output can be syled using 'Attr' (attribute)
--   values in the "Graphics.Vty.Attributes" module.
--
-- Vty uses threads internally, so programs made with Vty must be
-- compiled with the threaded runtime using the GHC @-threaded@ option.
--
-- As a small example, the following program demonstrates the use of Vty
-- on a Unix system using the @vty-unix@ package:
--
-- @
--  import "Graphics.Vty"
--  import "Graphics.Vty.Platform.Unix" ('Graphics.Vty.Platform.Unix.mkVty')
--
--  main = do
--      vty <- 'mkVty' 'defaultConfig' Nothing
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
  , setWindowTitle
  , installCustomWidthTable
  , mkVtyFromPair
  , module Graphics.Vty.Config
  , module Graphics.Vty.Input
  , module Graphics.Vty.Input.Events
  , module Graphics.Vty.Output
  , module Graphics.Vty.Picture
  , module Graphics.Vty.Image
  , module Graphics.Vty.Attributes
  )
where

import Graphics.Vty.Config
import Graphics.Vty.Input
import Graphics.Vty.Input.Events
import Graphics.Vty.Output
import Graphics.Vty.Picture
import Graphics.Vty.Image
import Graphics.Vty.Attributes
import Graphics.Vty.UnicodeWidthTable.IO
import Graphics.Vty.UnicodeWidthTable.Install

import qualified Control.Exception as E
import Control.Monad (when)
import Control.Concurrent.STM

import Data.IORef
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif

-- | A 'Vty' value represents a handle to the Vty library that the
-- application must create in order to use Vty.
--
-- The use of this library typically follows this process:
--
-- 1. Initialize Vty with the 'mkVty' implementation for your platform's
-- Vty package, or, more generically, with 'mkVtyFromPair'. This takes
-- control of (and set up) the terminal.
--
-- 2. Use 'update' to display a picture.
--
-- 3. Use 'nextEvent' to get the next input event.
--
-- 4. Depending on the event, go to 2 or 5.
--
-- 5. Shutdown Vty and restore the terminal state with 'shutdown'. At
-- this point the 'Vty' handle cannot be used again.
--
-- Operations on Vty handles are not thread-safe.
data Vty =
    Vty { update :: Picture -> IO ()
        -- ^ Output the given 'Picture' to the terminal.
        , nextEvent :: IO Event
        -- ^ Return the next 'Event' or block until one becomes
        -- available.
        , nextEventNonblocking :: IO (Maybe Event)
        -- ^ Non-blocking version of 'nextEvent'.
        , inputIface :: Input
        -- ^ The input interface. See 'Input'.
        , outputIface :: Output
        -- ^ The output interface. See 'Output'.
        , refresh :: IO ()
        -- ^ Refresh the display. If other programs output to the
        -- terminal and mess up the display then the application might
        -- want to force a refresh using this function.
        , shutdown :: IO ()
        -- ^ Clean up after vty. A call to this function is necessary to
        -- cleanly restore the terminal state before application exit.
        -- The above methods will throw an exception if executed after
        -- this is executed. Idempotent.
        , isShutdown :: IO Bool
        }

installCustomWidthTable :: Maybe FilePath -> Maybe String -> [(String, String)] -> IO ()
installCustomWidthTable logPath tblName widthMaps = do
    let doLog s = case logPath of
                      Nothing -> return ()
                      Just path -> appendFile path $ "installWidthTable: " <> s <> "\n"

    customInstalled <- isCustomTableReady
    when (not customInstalled) $ do
        case tblName of
            Nothing ->
                doLog "No terminal name given in the configuration, skipping load"
            Just name ->
                case lookup name widthMaps of
                    Nothing ->
                        doLog $ "Width table " <> show name <> " not found in custom character width mapping list"
                    Just path -> do
                        tableResult <- E.try $ readUnicodeWidthTable path
                        case tableResult of
                            Left (e::E.SomeException) ->
                                doLog $ "Error reading custom character width table " <>
                                        "at " <> show path <> ": " <> show e
                            Right (Left msg) ->
                                doLog $ "Error reading custom character width table " <>
                                        "at " <> show path <> ": " <> msg
                            Right (Right table) -> do
                                installResult <- E.try $ installUnicodeWidthTable table
                                case installResult of
                                    Left (e::E.SomeException) ->
                                        doLog $ "Error installing unicode table (" <>
                                                show path <> ": " <> show e
                                    Right () ->
                                        doLog $ "Successfully installed Unicode width table " <>
                                                " from " <> show path

mkVtyFromPair :: Input -> Output -> IO Vty
mkVtyFromPair input out = do
    reserveDisplay out

    shutdownVar <- newTVarIO False
    let shutdownIo = do
            alreadyShutdown <- atomically $ swapTVar shutdownVar True
            when (not alreadyShutdown) $ do
                shutdownInput input
                releaseDisplay out
                releaseTerminal out

        shutdownStatus = readTVarIO shutdownVar

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

        innerRefresh = do
            writeIORef lastUpdateRef Nothing
            bounds <- displayBounds out
            dc <- displayContext out bounds
            writeIORef (assumedStateRef $ contextDevice dc) initialAssumedState
            mPic <- readIORef lastPicRef
            maybe (return ()) innerUpdate mPic

        mkResize = uncurry EvResize <$> displayBounds out

        translateInternalEvent ResumeAfterInterrupt = mkResize
        translateInternalEvent (InputEvent e) = return e

        gkey = do
            e <- atomically $ readTChan $ eventChannel input
            translateInternalEvent e
        gkey' = do
            mEv <- atomically $ tryReadTChan $ eventChannel input
            case mEv of
                Just e  -> Just <$> translateInternalEvent e
                Nothing -> return Nothing

    return $ Vty { update = innerUpdate
                 , nextEvent = gkey
                 , nextEventNonblocking = gkey'
                 , inputIface = input
                 , outputIface = out
                 , refresh = innerRefresh
                 , shutdown = shutdownIo
                 , isShutdown = shutdownStatus
                 }

-- | Set the terminal window title string.
setWindowTitle :: Vty -> String -> IO ()
setWindowTitle vty title =
    setOutputWindowTitle (outputIface vty) title
