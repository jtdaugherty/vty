{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- | The input layer used to be a single function that correctly accounted for the non-threaded
-- runtime by emulating the terminal VMIN adn VTIME handling. This has been removed and replace with
-- a more straightforward parser. The non-threaded runtime is no longer supported.
--
-- This is an example of an algorithm where code coverage could be high, even 100%, but the
-- behavior is still under tested. I should collect more of these examples...
module Graphics.Vty.Input.Loop where

import Graphics.Vty.Config
import Graphics.Vty.Input.Classify
import Graphics.Vty.Input.Events

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad (when, mzero, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..))

import Data.Char
import Data.IORef
import Data.Word (Word8)

import Foreign ( allocaArray, peekArray, Ptr )
import Foreign.C.Types (CInt(..))

import System.IO
import System.Posix.IO (fdReadBuf)
import System.Posix.Terminal
import System.Posix.Types (Fd(..))

import Text.Printf (hPrintf)

data Input = Input
    { -- | Channel of events direct from input processing. Unlike 'nextEvent' this will not refresh
      -- the display if the next event is an 'EvResize'.
      _eventChannel  :: Chan Event
      -- | Shuts down the input processing. This should return the terminal input state to before
      -- the input initialized.
    , shutdownInput :: IO ()
      -- | Changes to this value are reflected after the next event.
    , _configRef :: IORef Config
      -- | File descriptor used for input.
    , _inputFd :: Fd
      -- | input debug log
    , _inputDebug :: Maybe Handle
    }

makeLenses ''Input

data InputBuffer = InputBuffer
    { _ptr :: Ptr Word8
    , _size :: Int
    }

makeLenses ''InputBuffer

data InputState = InputState
    { _unprocessedBytes :: String
    , _appliedConfig :: Config
    , _inputBuffer :: InputBuffer
    , _stopRequestRef :: IORef Bool
    , _classifier :: String -> KClass
    }

makeLenses ''InputState

type InputM a = StateT InputState (ReaderT Input IO) a

logMsg :: String -> InputM ()
logMsg msg = do
    d <- view inputDebug
    case d of
        Nothing -> return ()
        Just h -> liftIO $ hPutStrLn h msg >> hFlush h

-- this must be run on an OS thread dedicated to this input handling.
-- otherwise the terminal timing read behavior will block the execution of the lightweight threads.
loopInputProcessor :: InputM ()
loopInputProcessor = do
    readFromDevice >>= addBytesToProcess
    validEvents <- many parseEvent
    forM_ validEvents emit
    dropInvalid
    stopIfRequested <|> loopInputProcessor

addBytesToProcess :: String -> InputM ()
addBytesToProcess block = unprocessedBytes <>= block

emit :: Event -> InputM ()
emit event = do
    logMsg $ "parsed event: " ++ show event
    view eventChannel >>= liftIO . flip writeChan event

-- The timing requirements are assured by the VMIN and VTIME set for the device.
--
-- Precondition: Under the threaded runtime. Only current use is from a forkOS thread. That case
-- satisfies precondition.
-- TODO: When under the non-threaded runtime emulate VMIN and VTIME
readFromDevice :: InputM String
readFromDevice = do
    newConfig <- view configRef >>= liftIO . readIORef
    oldConfig <- use appliedConfig
    fd <- view inputFd
    when (newConfig /= oldConfig) $ do
        liftIO $ applyTimingConfig fd newConfig
        appliedConfig .= newConfig
    bufferPtr <- use $ inputBuffer.ptr
    maxBytes  <- use $ inputBuffer.size
    stringRep <- liftIO $ do
        bytesRead <- fdReadBuf fd bufferPtr (fromIntegral maxBytes)
        if bytesRead > 0
        then fmap (map $ chr . fromIntegral) $ peekArray (fromIntegral bytesRead) bufferPtr
        else return []
    logMsg $ "input bytes: " ++ show stringRep
    return stringRep

applyTimingConfig :: Fd -> Config -> IO ()
applyTimingConfig fd config =
    let vtime = min 255 $ singleEscPeriod config `div` 100000
    in setTermTiming fd 1 vtime

parseEvent :: InputM Event
parseEvent = do
    c <- use classifier
    b <- use unprocessedBytes
    case c b of
        Valid e remaining -> do
            unprocessedBytes .= remaining
            return e
        _                   -> mzero 

dropInvalid :: InputM ()
dropInvalid = do
    c <- use classifier
    b <- use unprocessedBytes
    when (c b == Invalid) $ unprocessedBytes .= []

stopIfRequested :: InputM ()
stopIfRequested = do
    True <- (liftIO . readIORef) =<< use stopRequestRef
    return ()

runInputProcessorLoop :: ClassifyTable -> Input -> IORef Bool -> IO ()
runInputProcessorLoop classifyTable input stopFlag = do
    let bufferSize = 1024
    allocaArray bufferSize $ \(bufferPtr :: Ptr Word8) -> do
        s0 <- InputState [] <$> readIORef (_configRef input)
                            <*> pure (InputBuffer bufferPtr bufferSize)
                            <*> pure stopFlag
                            <*> pure (classify classifyTable)
        runReaderT (evalStateT loopInputProcessor s0) input

attributeControl :: Fd -> IO (IO (), IO ())
attributeControl fd = do
    original <- getTerminalAttributes fd
    let vtyMode = foldl withoutMode original [ StartStopOutput, KeyboardInterrupts
                                             , EnableEcho, ProcessInput, ExtendedFunctions
                                             ]
    let setAttrs = setTerminalAttributes fd vtyMode Immediately
        unsetAttrs = setTerminalAttributes fd original Immediately
    return (setAttrs,unsetAttrs)

logClassifyTable :: Input -> ClassifyTable -> IO()
logClassifyTable input classifyTable = case _inputDebug input of
    Nothing -> return ()
    Just h  -> do
        forM_ classifyTable $ \i -> case i of
            (inBytes, EvKey k mods) -> hPrintf h "map %s %s %s\n" (show inBytes)
                                                                  (show k)
                                                                  (show mods)
            _ -> return ()

initInputForFd :: Config -> ClassifyTable -> Fd -> IO Input
initInputForFd config classifyTable inFd = do
    applyTimingConfig inFd config
    stopFlag <- newIORef False
    input <- Input <$> newChan
                   <*> pure (writeIORef stopFlag True)
                   <*> newIORef config
                   <*> pure inFd
                   <*> maybe (return Nothing)
                             (\f -> Just <$> openFile f AppendMode)
                             (debugLog config)
    logClassifyTable input classifyTable
    _ <- forkOS $ runInputProcessorLoop classifyTable input stopFlag
    return input

foreign import ccall "vty_set_term_timing" setTermTiming :: Fd -> Int -> Int -> IO ()
