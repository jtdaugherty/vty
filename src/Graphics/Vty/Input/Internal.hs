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
    { -- | Channel of events direct from input processing. Unlike 'next_event' this will not refresh
      -- the display if the next event is an 'EvResize'.
      _event_channel  :: Chan Event
      -- | Shuts down the input processing. This should return the terminal input state to before
      -- the input initialized.
    , shutdown_input :: IO ()
      -- | Changes to this value are reflected after the next event.
    , _config_ref :: IORef Config
      -- | File descriptor used for input.
    , _input_fd :: Fd
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
    { _unprocessed_bytes :: String
    , _applied_config :: Config
    , _input_buffer :: InputBuffer
    , _stop_request_ref :: IORef Bool
    , _classifier :: String -> KClass
    }

makeLenses ''InputState

type InputM a = StateT InputState (ReaderT Input IO) a

-- this must be run on an OS thread dedicated to this input handling.
-- otherwise the terminal timing read behavior will block the execution of the lightweight threads.
loop_input_processor :: InputM ()
loop_input_processor = do
    read_from_device >>= add_bytes_to_process
    valid_events <- many parse_event
    forM_ valid_events emit
    drop_invalid
    stop_if_requested <|> loop_input_processor

add_bytes_to_process :: String -> InputM ()
add_bytes_to_process block = unprocessed_bytes <>= block

emit :: Event -> InputM ()
emit event = view event_channel >>= liftIO . flip writeChan event

-- There should be two versions of this method:
-- 1. using VMIN and VTIME when under the threaded runtime
-- 2. emulating VMIN and VTIME in userspace when under the non-threaded runtime.
read_from_device :: InputM String
read_from_device = do
    new_config <- view config_ref >>= liftIO . readIORef
    old_config <- use applied_config
    fd <- view input_fd
    when (new_config /= old_config) $ do
        liftIO $ apply_timing_config fd new_config
        applied_config .= new_config
    buffer_ptr <- use $ input_buffer.ptr
    max_bytes  <- use $ input_buffer.size
    liftIO $ do
        bytes_read <- fdReadBuf fd buffer_ptr (fromIntegral max_bytes)
        if bytes_read > 0
        then fmap (map $ chr . fromIntegral) $ peekArray (fromIntegral bytes_read) buffer_ptr
        else return []

apply_timing_config :: Fd -> Config -> IO ()
apply_timing_config fd config =
    let vtime = min 255 $ singleEscPeriod config `div` 100000
    in set_term_timing fd 1 vtime

parse_event :: InputM Event
parse_event = do
    c <- use classifier
    b <- use unprocessed_bytes
    case c b of
        Valid e remaining -> do
            unprocessed_bytes .= remaining
            return e
        _                   -> mzero 

drop_invalid :: InputM ()
drop_invalid = do
    c <- use classifier
    b <- use unprocessed_bytes
    when (c b == Invalid) $ unprocessed_bytes .= []

stop_if_requested :: InputM ()
stop_if_requested = do
    True <- (liftIO . readIORef) =<< use stop_request_ref
    return ()

-- This makes a kind of tri. Has space efficiency issues with large input blocks.
-- Likely building a parser and just applying that would be better.
-- I did not write this so I might just rewrite it for better understanding. Not the best of
-- reasons...
-- TODO: measure and rewrite if required.
compile :: ClassifyTable -> [Char] -> KClass
compile table = cl' where
    -- take all prefixes and create a set of these
    prefix_set = S.fromList $ concatMap (init . inits . fst) $ table
    event_for_input = M.fromList table
    cl' [] = Prefix
    cl' input_block = case M.lookup input_block event_for_input of
            Just e -> Valid e []
            Nothing -> case S.member input_block prefix_set of
                True -> Prefix
                -- if the input_block is exactly what is expected for an event then consume the whole
                -- block and return the event
                -- look up progressively smaller tails of the input block until an event is found
                -- The assumption is that the event that consumes the most input bytes should be
                -- produced.
                -- The test verify_full_syn_input_to_event_2x verifies this.
                -- H: There will always be one match. The prefix_set contains, by definition, all
                -- prefixes of an event. 
                False ->
                    let input_tails = init $ tail $ tails input_block
                    in case mapMaybe (\s -> (,) s `fmap` M.lookup s event_for_input) input_tails of
                        (s,e) : _ -> Valid e (drop (length s) input_block)
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

run_input_processor_loop :: ClassifyTable -> Input -> IORef Bool -> IO ()
run_input_processor_loop classify_table input stop_flag = do
    let buffer_size = 1024
    allocaArray buffer_size $ \(buffer_ptr :: Ptr Word8) -> do
        s0 <- InputState [] <$> readIORef (_config_ref input)
                            <*> pure (InputBuffer buffer_ptr buffer_size)
                            <*> pure stop_flag
                            <*> pure (classify classify_table)
        runReaderT (evalStateT loop_input_processor s0) input

attributeControl :: Fd -> IO (IO (), IO ())
attributeControl fd = do
    original <- getTerminalAttributes fd
    let vtyMode = foldl withoutMode original [ StartStopOutput, KeyboardInterrupts
                                             , EnableEcho, ProcessInput, ExtendedFunctions
                                             ]
    let set_attrs = setTerminalAttributes fd vtyMode Immediately
        unset_attrs = setTerminalAttributes fd original Immediately
    return (set_attrs,unset_attrs)

-- This is an example of an algorithm where code coverage could be high, even 100%, but the
-- algorithm still under tested. I should collect more of these examples...
initInputForFd :: Config -> ClassifyTable -> Fd -> IO Input
initInputForFd config classify_table in_fd = do
    apply_timing_config in_fd config
    stop_flag <- newIORef False
    input <- Input <$> newChan
                   <*> pure (writeIORef stop_flag True)
                   <*> newIORef config
                   <*> pure in_fd
    _ <- forkOS $ run_input_processor_loop classify_table input stop_flag
    return input

foreign import ccall "vty_set_term_timing" set_term_timing :: Fd -> Int -> Int -> IO ()
