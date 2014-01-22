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
import Control.Exception (try, IOException)
import Control.Monad (when, void, mzero, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.Reader (ReaderT(..))

import Data.Char
import Data.Default
import Data.IORef
import Data.List( inits )
import qualified Data.Map as M( fromList, lookup )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S( fromList, member )
import Data.Word

import Foreign ( allocaArray, peekArray, Ptr )
import Foreign.C.Types (CInt(..))

import System.Posix.IO ( fdReadBuf
                       , setFdOption
                       , FdOption(..)
                       )
import System.Posix.Types (Fd(..))

data Config = Config
    { singleEscPeriod :: Int -- ^ The default is 100000 microseconds or 0.1 seconds.
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
    = Valid Key [Modifier] [Char]
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
        fmap (map $ chr . fromIntegral) $ peekArray (fromIntegral bytes_read) buffer_ptr

apply_timing_config :: Fd -> Config -> IO ()
apply_timing_config fd config =
    let vtime = min 255 $ singleEscPeriod config `div` 100000
    in set_term_timing fd 1 vtime

parse_event :: InputM Event
parse_event = do
    c <- use classifier
    b <- use unprocessed_bytes
    case c b of
        Valid k m remaining -> do
            unprocessed_bytes .= remaining
            return $ EvKey k m
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
    -- create a map from strings to event
    event_for_input = flip M.lookup (M.fromList table)
    cl' [] = Prefix
    cl' input_block = case event_for_input input_block of
            Just (EvKey k m) -> Valid k m []
            Nothing -> case S.member input_block prefix_set of
                True -> Prefix
                -- if the input_block is exactly what is expected for an event then consume the whole
                -- block and return the event
                -- look up progressively large prefixes of the input block until an event is found
                -- H: There will always be one match. The prefix_set contains, by definition, all
                -- prefixes of an event. 
                False ->
                    let input_prefixes = init $ inits input_block
                    in case mapMaybe (\s -> (,) s `fmap` event_for_input s) input_prefixes of
                        (s,EvKey k m) : _ -> Valid k m (drop (length s) input_block)
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
    Just (unicodeChar, _) -> Valid (KChar unicodeChar) [] []
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

-- I gave a quick shot at replacing this code with some that removed the "odd" bits. The "obvious"
-- changes all failed testing. This is timing sensitive code.
-- I now think I can replace this wil code that makes the time sensitivity explicit. I am waiting
-- until I have a good set of characterization tests to verify the input to event timing is still
-- correct for a user. I estimate the current tests cover ~70% of the required cases.
--
-- This is an example of an algorithm where code coverage could be high, even 100%, but the
-- algorithm still under tested. I should collect more of these examples...
initInputForFd :: IORef Config -> ClassifyTable -> Fd -> IO Input
initInputForFd config_ref classify_table input_fd = do
    stop_flag <- newIORef False
    input <- Input <$> newChan
                   <*> pure (writeIORef stop_flag True)
                   <*> pure config_ref
                   <*> pure input_fd
    _ <- forkOS $ run_input_processor_loop classify_table input stop_flag
    return input

foreign import ccall "vty_set_term_timing" set_term_timing :: Fd -> Int -> Int -> IO ()
