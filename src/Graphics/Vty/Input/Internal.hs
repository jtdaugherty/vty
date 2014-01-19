-- Much of the input layer used to be in a single module with a few large functions. I've refactored
-- the input layer into many small bits. Now, I think, the code is a better position to be
-- incrementally refined. Still, until there are proper tests in place much of the refinement must
-- wait.
module Graphics.Vty.Input.Internal where

import Graphics.Vty.Input.Events

import Codec.Binary.UTF8.Generic (decode)
import Control.Concurrent
import Control.Exception (try, IOException)
import Control.Monad (when, void)

import Data.Char
import Data.List( inits )
import qualified Data.Map as M( fromList, lookup )
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S( fromList, member )
import Data.Word

import Foreign ( alloca, poke, peek, Ptr )
import Foreign.C.Types (CInt(..))

import System.Posix.IO ( fdReadBuf
                       , setFdOption
                       , FdOption(..)
                       )
import System.Posix.Types (Fd(..))

data Input = Input
    { -- | Channel of events direct from input processing. Unlike 'next_event' this will not refresh
      -- the display if the next event is an 'EvResize'.
      event_channel  :: Chan Event
      -- | Shuts down the input processing. This should return the terminal input state to before
      -- the input initialized.
    , shutdown_input :: IO ()
    }

data KClass
    = Valid Key [Modifier]
    | Invalid
    | EndLoop
    | Prefix
    | MisPfx Key [Modifier] [Char]
    deriving(Show)

-- The input uses two magic character:
--
--  * '\xFFFD' for "stop processing"
--  * '\xFFFE' for "previous input chunk is complete" Which is used to differentiate a single ESC
--  from using ESC as meta or to indicate a control sequence.
inputToEventThread :: (String -> KClass) -> Chan Char -> Chan Event -> IO ()
inputToEventThread classifier inputChannel eventChannel = loop []
    where loop kb = case classifier kb of
            Prefix       -> do
                c <- readChan inputChannel
                if c == '\xFFFD'
                    then return ()
                    else loop (kb ++ [c])
            -- drop the entirety of invalid sequences. Probably better to drop only the first
            -- character. However, the read behavior of terminals (a single read corresponds to the
            -- bytes of a single event) might mean this results in quicker error recovery from a
            -- users perspective.
            Invalid      -> do
                c <- readChan inputChannel
                if c == '\xFFFD'
                    then return ()
                    else loop [c]
            EndLoop      -> return ()
            MisPfx k m s -> writeChan eventChannel (EvKey k m) >> loop s
            Valid k m    -> writeChan eventChannel (EvKey k m) >> loop ""

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
    cl' input_block = case S.member input_block prefix_set of
            True -> Prefix
            -- if the input_block is exactly what is expected for an event then consume the whole
            -- block and return the event
            False -> case event_for_input input_block of
                Just (EvKey k m) -> Valid k m
                -- look up progressively large prefixes of the input block until an event is found
                -- H: There will always be one match. The prefix_set contains, by definition, all
                -- prefixes of an event. 
                Nothing -> 
                    let input_prefixes = init $ inits input_block
                    in case mapMaybe (\s -> (,) s `fmap` event_for_input s) input_prefixes of
                        (s,EvKey k m) : _ -> MisPfx k m (drop (length s) input_block)
                        -- neither a prefix or a full event. Might be interesting to log.
                        [] -> error $ show input_block

classify, classifyTab :: ClassifyTable -> [Char] -> KClass

-- As soon as
classify _table "\xFFFD" = EndLoop
classify _table "\xFFFE" = Invalid
classify _table s@(c:_) | ord c >= 0xC2
    = if utf8Length (ord c) > length s then Prefix else classifyUtf8 s -- beginning of an utf8 sequence
classify table other
    = classifyTab table other

classifyUtf8 :: [Char] -> KClass
classifyUtf8 s = case decode ((map (fromIntegral . ord) s) :: [Word8]) of
    Just (unicodeChar, _) -> Valid (KChar unicodeChar) []
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

-- I gave a quick shot at replacing this code with some that removed the "odd" bits. The "obvious"
-- changes all failed testing. This is timing sensitive code.
-- I now think I can replace this wil code that makes the time sensitivity explicit. I am waiting
-- until I have a good set of characterization tests to verify the input to event timing is still
-- correct for a user. I estimate the current tests cover ~70% of the required cases.
--
-- This is an example of an algorithm where code coverage could be high, even 100%, but the
-- algorithm still under tested. I should collect more of these examples...
initInputForFd :: Int -> ClassifyTable -> Fd -> IO (Chan Event, IO ())
initInputForFd escDelay classify_table input_fd = do
    inputChannel <- newChan
    eventChannel <- newChan
    hadInput <- newEmptyMVar
    let finishAtomicInput = writeChan inputChannel '\xFFFE'
        inputThread :: IO ()
        inputThread = do
            _ <- alloca $ \(input_buffer :: Ptr Word8) -> do
                let loop = do
                        setFdOption input_fd NonBlockingRead False
                        threadWaitRead input_fd
                        setFdOption input_fd NonBlockingRead True
                        _ <- try readAll :: IO (Either IOException ())
                        when (escDelay == 0) finishAtomicInput
                        loop
                    readAll = do
                        poke input_buffer 0
                        bytes_read <- fdReadBuf input_fd input_buffer 1
                        input_char <- fmap (chr . fromIntegral) $ peek input_buffer
                        when (bytes_read > 0) $ do
                            _ <- tryPutMVar hadInput () -- signal input
                            writeChan inputChannel input_char
                            readAll
                loop
            return ()
        -- If there is no input for some time, this thread puts '\xFFFE' in the
        -- inputChannel.
        noInputThread :: IO ()
        noInputThread = when (escDelay > 0) loop
            where loop = do
                    takeMVar hadInput -- wait for some input
                    threadDelay escDelay -- microseconds
                    hadNoInput <- isEmptyMVar hadInput -- no input yet?
                    -- TODO(corey): there is a race between here and the inputThread.
                    when hadNoInput finishAtomicInput
                    loop
        classifier = classify classify_table
    eventThreadId <- forkIO $ void $ inputToEventThread classifier inputChannel eventChannel
    inputThreadId <- forkIO $ inputThread
    noInputThreadId <- forkIO $ noInputThread
    -- TODO(corey): killThread is a bit risky for my tastes.
    -- H - somewhat mitigated by sending a magic terminate character?
    let shutdown_event_processing = do
            writeChan inputChannel '\xFFFD' 
            killThread noInputThreadId
            killThread eventThreadId
            killThread inputThreadId
    return (eventChannel, shutdown_event_processing)

foreign import ccall "vty_set_term_timing" set_term_timing :: Fd -> IO ()
