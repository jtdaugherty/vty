{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- | The input layer used to be a single function that correctly accounted for the non-threaded
-- runtime by emulating the terminal VMIN adn VTIME handling. This has been removed and replace with
-- a more straightforward parser. The non-threaded runtime is no longer supported.
module Graphics.Vty.Input.Internal where

import Graphics.Vty.Input.Events

import Codec.Binary.UTF8.Generic (decode)
import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad (when, mzero, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..))

import Data.Char
import Data.Default
import Data.IORef
import Data.List(tails,inits)
import qualified Data.Map as M( fromList, lookup )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S( fromList, member )
import Data.Word

import Foreign ( allocaArray, peekArray, Ptr )
import Foreign.C.Types (CInt(..))

import System.Posix.IO (fdReadBuf)
import System.Posix.Terminal
import System.Posix.Types (Fd(..))

data Config = Config
    { singleEscPeriod :: Int -- ^ AKA VTIME. The default is 100000 microseconds or 0.1 seconds.
    } deriving (Show, Eq)

instance Default Config where
    def = Config
        { singleEscPeriod = 100000
        }

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
    }

makeLenses ''Input

data KClass
    = Valid Event [Char]
    | Invalid
    | Prefix
    deriving(Show, Eq)

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
emit event = view eventChannel >>= liftIO . flip writeChan event

-- There should be two versions of this method:
-- 1. using VMIN and VTIME when under the threaded runtime
-- 2. emulating VMIN and VTIME in userspace when under the non-threaded runtime.
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
    liftIO $ do
        bytesRead <- fdReadBuf fd bufferPtr (fromIntegral maxBytes)
        if bytesRead > 0
        then fmap (map $ chr . fromIntegral) $ peekArray (fromIntegral bytesRead) bufferPtr
        else return []

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

-- This makes a kind of tri. Has space efficiency issues with large input blocks.
-- Likely building a parser and just applying that would be better.
-- I did not write this so I might just rewrite it for better understanding. Not the best of
-- reasons...
-- TODO: measure and rewrite if required.
compile :: ClassifyTable -> [Char] -> KClass
compile table = cl' where
    -- take all prefixes and create a set of these
    prefixSet = S.fromList $ concatMap (init . inits . fst) $ table
    eventForInput = M.fromList table
    cl' [] = Prefix
    cl' inputBlock = case M.lookup inputBlock eventForInput of
            Just e -> Valid e []
            Nothing -> case S.member inputBlock prefixSet of
                True -> Prefix
                -- if the inputBlock is exactly what is expected for an event then consume the whole
                -- block and return the event
                -- look up progressively smaller tails of the input block until an event is found
                -- The assumption is that the event that consumes the most input bytes should be
                -- produced.
                -- The test verifyFullSynInputToEvent2x verifies this.
                -- H: There will always be one match. The prefixSet contains, by definition, all
                -- prefixes of an event. 
                False ->
                    let inputTails = init $ tail $ tails inputBlock
                    in case mapMaybe (\s -> (,) s `fmap` M.lookup s eventForInput) inputTails of
                        (s,e) : _ -> Valid e (drop (length s) inputBlock)
                        -- neither a prefix or a full event. Might be interesting to log.
                        [] -> Invalid

classify, classifyTab :: ClassifyTable -> [Char] -> KClass

-- As soon as
classify _table s@(c:_) | ord c >= 0xC2
    = if utf8Length (ord c) > length s then Prefix else classifyUtf8 s -- beginning of an utf8 sequence
classify table other
    = classifyTab table other

classifyUtf8 :: [Char] -> KClass
classifyUtf8 s = case decode ((map (fromIntegral . ord) s) :: [Word8]) of
    Just (unicodeChar, _) -> Valid (EvKey (KChar unicodeChar) []) []
    _ -> Invalid -- something bad happened; just ignore and continue.

classifyTab table = compile table

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

utf8Length :: (Num t, Ord a, Num a) => a -> t
utf8Length c
    | c < 0x80 = 1
    | c < 0xE0 = 2
    | c < 0xF0 = 3
    | otherwise = 4

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

-- This is an example of an algorithm where code coverage could be high, even 100%, but the
-- algorithm still under tested. I should collect more of these examples...
initInputForFd :: Config -> ClassifyTable -> Fd -> IO Input
initInputForFd config classifyTable inFd = do
    applyTimingConfig inFd config
    stopFlag <- newIORef False
    input <- Input <$> newChan
                   <*> pure (writeIORef stopFlag True)
                   <*> newIORef config
                   <*> pure inFd
    _ <- forkOS $ runInputProcessorLoop classifyTable input stopFlag
    return input

foreign import ccall "vty_set_term_timing" setTermTiming :: Fd -> Int -> Int -> IO ()
