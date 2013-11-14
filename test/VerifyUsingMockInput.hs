{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module Main where

import Verify.Graphics.Vty.Terminal

import Data.List (intersperse)

import Graphics.Vty hiding (resize)
import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal
import Graphics.Vty.Input.Terminfo

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.IORef

import System.Console.Terminfo
import System.Posix.IO
import System.Posix.Types
import System.Timeout

import Test.Framework.Providers.SmallCheck
import Test.Framework
import Test.SmallCheck
import Test.SmallCheck.Series

-- processing a block of 16 chars is the largest I can do without taking too long to run the test.
max_block_size :: Int
max_block_size = 16

max_table_size :: Int
max_table_size = 28

forEachOf :: (Show a, Testable m b) => [a] -> (a -> b) -> Property m
forEachOf l = over (generate (\n -> take n l))

data InputEvent
    = Bytes String  -- | input sequence encoded as a string. Regardless, the input is read a byte at a time.
    | Delay Int     -- | microsecond delay
    deriving Show

type InputSpec = [InputEvent]

type ExpectedSpec = [Event]

synthesize_input :: InputSpec -> Fd -> IO ()
synthesize_input input out_handle = forM_ input f >> (void $ fdWrite out_handle "\xFFFD")
    where
        f (Bytes str) = void $ fdWrite out_handle str
        f (Delay t) = threadDelay t

min_detectable_delay :: Int
min_detectable_delay = 1000

min_timout :: Int
min_timout = 4000000

test_key_delay :: Int
test_key_delay = min_detectable_delay * 4

test_esc_sample_delay :: Int
test_esc_sample_delay = min_detectable_delay * 2

gen_events_using_io_actions :: Int -> IO () -> IO () -> IO ()
gen_events_using_io_actions max_duration input_action output_action = do
    let max_duration' = max min_timout max_duration
    read_complete <- newEmptyMVar
    write_complete <- newEmptyMVar
    _ <- forkIO $ input_action `finally` putMVar write_complete ()
    _ <- forkIO $ output_action `finally` putMVar read_complete ()
    Just () <- timeout max_duration' $ takeMVar write_complete
    Just () <- timeout max_duration' $ takeMVar read_complete
    return ()

assert_events_from_syn_input :: ClassifyTable -> InputSpec -> ExpectedSpec -> IO Bool
assert_events_from_syn_input table input_spec expected_events = do
    let max_duration = sum [t | Delay t <- input_spec] + min_detectable_delay
        event_count = length expected_events
    (output_fd, input_fd) <- createPipe
    (output, shutdown_input) <- initInputForFd test_esc_sample_delay table output_fd
    events_ref <- newIORef []
    let write_wait_close = do
            synthesize_input input_spec input_fd
            threadDelay min_detectable_delay
            shutdown_input
            threadDelay min_detectable_delay
            closeFd input_fd
            closeFd output_fd
    -- drain output pipe
    let read_events = read_loop event_count
        read_loop 0 = return ()
        read_loop n = do
            e <- readChan output
            modifyIORef events_ref ((:) e)
            read_loop (n - 1)
    gen_events_using_io_actions max_duration write_wait_close read_events
    out_events <- reverse <$> readIORef events_ref
    print (input_spec, expected_events, out_events)
    return $ out_events == expected_events

assert_events_from_input_block :: ClassifyTable -> InputSpec -> ExpectedSpec -> IO Bool
assert_events_from_input_block table input_spec expected_events = do
    let classifier = classify table
        max_duration = sum [t | Delay t <- input_spec] + min_detectable_delay
    input <- newChan
    output <- newChan
    gen_events_using_io_actions
        max_duration
        (write_input_spec_to_chan input_spec input)
        (inputToEventThread classifier input output)
    -- TODO: use STM TChan?
    -- assures reading "length expected_events" from the channel does not block.
    let min_event_count = length expected_events
    writeList2Chan output $ replicate min_event_count undefined
    out_events <- take min_event_count <$> getChanContents output
    return $ out_events == expected_events

write_input_spec_to_chan :: InputSpec -> Chan Char -> IO ()
write_input_spec_to_chan [] chan = do
    writeChan chan '\xFFFD'
write_input_spec_to_chan (Bytes str : input_spec') chan = do
    writeList2Chan chan str
    write_input_spec_to_chan input_spec' chan
write_input_spec_to_chan (Delay _t : input_spec') chan = do
    writeChan chan '\xFFFE'
    write_input_spec_to_chan input_spec' chan

newtype InputBlocksUsingTable event
    = InputBlocksUsingTable ([(String,event)] -> [(String, event)])

instance Show (InputBlocksUsingTable event) where
    show (InputBlocksUsingTable _g) = "InputBlocksUsingTable"

instance Monad m => Serial m (InputBlocksUsingTable event) where
    series = do
        n :: Int <- localDepth (const max_table_size) series
        return $ InputBlocksUsingTable $ \table -> concat (take n (selections table))
        where
            selections []     = []
            selections (x:xs) = let z = selections xs in [x] : (z ++ map ((:) x) z)

verify_simple_input_block_to_event :: Property IO
verify_simple_input_block_to_event = forAll $ \(InputBlocksUsingTable gen) -> do
    let input_seq = gen simple_chars
        input     = Bytes $ concat [s | (s,_) <- input_seq]
        events    = [e | (_,(k,ms)) <- input_seq, let e = EvKey k ms]
    monadic $ assert_events_from_input_block simple_chars [input] events

verify_keys_from_caps_table_block_to_event :: Property IO
verify_keys_from_caps_table_block_to_event = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminals_of_interest $ \term_name -> monadic $ do
        term <- setupTerm term_name
        let table         = caps_classify_table term keys_from_caps_table
            input_seq     = gen table
            events        = [e       | (_,e) <- input_seq]
            keydowns      = [Bytes s | (s,_) <- input_seq]
            input         = intersperse (Delay test_key_delay) keydowns
        assert_events_from_input_block (map_to_legacy_table table) input events

verify_simple_syn_input_to_event :: Property IO
verify_simple_syn_input_to_event = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table         = simple_chars
        input_seq     = gen table
        events        = [EvKey k ms | (_,(k,ms)) <- input_seq]
        keydowns      = [Bytes s    | (s,_) <- input_seq]
        input         = intersperse (Delay test_key_delay) keydowns
    assert_events_from_syn_input (concat ansi_classify_table) input events

verify_caps_syn_input_to_event :: Property IO
verify_caps_syn_input_to_event = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminals_of_interest $ \term_name -> monadic $ do
        term <- setupTerm term_name
        let table         = caps_classify_table term keys_from_caps_table
            input_seq     = gen table
            events        = [e       | (_,e) <- input_seq]
            keydowns      = [Bytes s | (s,_) <- input_seq]
            input         = intersperse (Delay test_key_delay) keydowns
        assert_events_from_syn_input (map_to_legacy_table table) input events

verify_special_syn_input_to_event :: Property IO
verify_special_syn_input_to_event = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table         = function_keys_1 ++ function_keys_2
        input_seq     = gen table
        events        = [EvKey k ms | (_,(k,ms)) <- input_seq]
        keydowns      = [Bytes s    | (s,_) <- input_seq]
        input         = intersperse (Delay test_key_delay) keydowns
    assert_events_from_syn_input (concat ansi_classify_table) input events

main :: IO ()
main = defaultMain
    [ testProperty "basic block generated from a single visible chars to event translation"
        verify_simple_input_block_to_event
    , testProperty "key sequences read from caps table map to expected events"
        verify_keys_from_caps_table_block_to_event
    , testProperty "synthesized typing from single visible chars translates to expected events"
        verify_simple_syn_input_to_event
    , testProperty "synthesized typing from keys from capabilities tables translates to expected events"
        verify_caps_syn_input_to_event
    , testProperty "synthesized typing from hard coded special keys translates to expected events"
        verify_special_syn_input_to_event
    ]

