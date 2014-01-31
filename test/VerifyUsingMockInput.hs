{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- Generate some input bytes and delays between blocks of input bytes. Verify the events produced
 - are as expected.
 -}
module Main where

import Verify.Graphics.Vty.Output

import Data.List (intersperse)

import Graphics.Vty hiding (resize)
import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Internal
import Graphics.Vty.Input.Terminfo

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens ((^.))
import Control.Monad

import Data.Default
import Data.IORef

import System.Console.Terminfo
import System.Posix.IO
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types
import System.Timeout

import Test.Framework.Providers.SmallCheck
import Test.Framework
import Test.SmallCheck
import Test.SmallCheck.Series

import Text.Printf

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
min_detectable_delay = 4000

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
    _ <- forkOS $ input_action `finally` putMVar write_complete ()
    _ <- forkOS $ output_action `finally` putMVar read_complete ()
    Just () <- timeout max_duration' $ takeMVar write_complete
    Just () <- timeout max_duration' $ takeMVar read_complete
    return ()

compare_events :: (Show a1, Show a, Eq a1) => a -> [a1] -> [a1] -> IO Bool
compare_events input_spec expected_events out_events = compare_events' expected_events out_events
    where
        compare_events' [] []         = return True
        compare_events' [] out_events' = do
            printf "extra events %s\n" (show out_events') :: IO ()
            return False
        compare_events' expected_events' [] = do
            printf "events %s were not produced for input %s\n" (show expected_events') (show input_spec) :: IO ()
            printf "expected events %s\n" (show expected_events) :: IO ()
            printf "received events %s\n" (show out_events) :: IO ()
            return False
        compare_events' (e : expected_events') (o : out_events')
            | e == o    = compare_events' expected_events' out_events'
            | otherwise = do
                printf "%s expected not %s for input %s\n" (show e) (show o) (show input_spec) :: IO ()
                printf "expected events %s\n" (show expected_events) :: IO ()
                printf "received events %s\n" (show out_events) :: IO ()
                return False

assert_events_from_syn_input :: ClassifyTable -> InputSpec -> ExpectedSpec -> IO Bool
assert_events_from_syn_input table input_spec expected_events = do
    let max_duration = sum [t | Delay t <- input_spec] + min_detectable_delay
        event_count = length expected_events
    (write_fd, read_fd) <- openPseudoTerminal
    (set_term_attr,_) <- attributeControl read_fd
    set_term_attr
    input <- initInputForFd def table read_fd
    events_ref <- newIORef []
    let write_wait_close = do
            synthesize_input input_spec write_fd
            threadDelay min_detectable_delay
            shutdown_input input
            threadDelay min_detectable_delay
            closeFd write_fd
            closeFd read_fd
    -- drain output pipe
    let read_events = read_loop event_count
        read_loop 0 = return ()
        read_loop n = do
            e <- readChan $ input^.event_channel
            modifyIORef events_ref ((:) e)
            read_loop (n - 1)
    gen_events_using_io_actions max_duration write_wait_close read_events
    out_events <- reverse <$> readIORef events_ref
    compare_events input_spec expected_events out_events

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

verify_visible_syn_input_to_event :: Property IO
verify_visible_syn_input_to_event = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table         = visible_chars
        input_seq     = gen table
        events        = map snd input_seq
        keydowns      = map (Bytes . fst) input_seq
        input         = intersperse (Delay test_key_delay) keydowns ++ [Delay test_key_delay]
    assert_events_from_syn_input universal_table input events

verify_caps_syn_input_to_event :: Property IO
verify_caps_syn_input_to_event = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminals_of_interest $ \term_name -> monadic $ do
        term <- setupTerm term_name
        let table         = caps_classify_table term keys_from_caps_table
            input_seq     = gen table
            events        = map snd input_seq
            keydowns      = map (Bytes . fst) input_seq
            input         = intersperse (Delay test_key_delay) keydowns ++ [Delay test_key_delay]
        assert_events_from_syn_input table input events

verify_special_syn_input_to_event :: Property IO
verify_special_syn_input_to_event = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table         = special_support_keys
        input_seq     = gen table
        events        = map snd input_seq
        keydowns      = map (Bytes . fst) input_seq
        input         = intersperse (Delay test_key_delay) keydowns ++ [Delay test_key_delay]
    assert_events_from_syn_input universal_table input events

verify_full_syn_input_to_event :: Property IO
verify_full_syn_input_to_event = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminals_of_interest $ \term_name -> monadic $ do
        term <- setupTerm term_name
        let table         = classify_table_for_term term_name term
            input_seq     = gen table
            events        = map snd input_seq
            keydowns      = map (Bytes . fst) input_seq
            input         = intersperse (Delay test_key_delay) keydowns ++ [Delay test_key_delay]
        assert_events_from_syn_input table input events

verify_full_syn_input_to_event_2x :: Property IO
verify_full_syn_input_to_event_2x = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminals_of_interest $ \term_name -> monadic $ do
        term <- setupTerm term_name
        let table         = classify_table_for_term term_name term
            input_seq     = gen table
            events        = concatMap ((\s -> [s,s]) . snd) input_seq
            keydowns      = map (Bytes . (\s -> s ++ s) . fst) input_seq
            input         = intersperse (Delay test_key_delay) keydowns ++ [Delay test_key_delay]
        assert_events_from_syn_input table input events

main :: IO ()
main = defaultMain
    [ testProperty "synthesized typing of single visible chars translates to expected events"
        verify_visible_syn_input_to_event
    , testProperty "synthesized typing of keys from capabilities tables translates to expected events"
        verify_caps_syn_input_to_event
    , testProperty "synthesized typing of hard coded special keys translates to expected events"
        verify_special_syn_input_to_event
    , testProperty "synthesized typing of any key in the table translates to its paired event"
        verify_full_syn_input_to_event
    , testProperty "synthesized typing of 2x any key in the table translates to 2x paired event"
        verify_full_syn_input_to_event_2x
    ]

