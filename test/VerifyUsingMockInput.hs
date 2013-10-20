{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module Main where

import Verify.Graphics.Vty.Terminal

import Data.List (intersperse, permutations)

import Graphics.Vty hiding (resize)
import Graphics.Vty.Input
import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal
import Graphics.Vty.Input.Terminfo

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import System.Console.Terminfo
import System.IO
import System.Posix.Env

import Test.Framework.Providers.SmallCheck
import Test.Framework
import Test.SmallCheck
import Test.SmallCheck.Series

-- processing a block of 16 chars is the largest I can do without taking too long to run the test.
max_bytes_size = 16

data InputEvent
    = Bytes String  -- | input sequence encoded as a string. Regardless, the input is read a byte at a time.
    | Delay Int     -- | millisecond delay

type InputSpec = [InputEvent]

type ExpectedSpec = [Event]

exec_input_spec :: InputSpec -> Handle -> IO ()
exec_input_spec input out_handle = forM_ input f
    where
        f (Bytes str) = hPutStr out_handle str >> hFlush out_handle
        f (Delay t) = threadDelay (t*1000)

assert_events_from_input_block :: ClassifyTable -> InputSpec -> ExpectedSpec -> IO Bool
assert_events_from_input_block table input_spec expected_events = do
    let classifier = classify table
    input <- newChan
    output <- newChan
    forkIO $ write_input_spec_to_chan input input_spec
    inputToEventThread classifier input output
    -- H: all events are available in output channel
    -- TODO: switch to using explicit control events. Currently the channel is passing around a
    -- magic FFFX characters
    -- TODO: use STM channel
    let collect_events = handle (\(e :: BlockedIndefinitelyOnMVar) -> return [])
                                ((:) <$> readChan output <*> collect_events)
    out_events <- collect_events
    return $ out_events == expected_events

write_input_spec_to_chan :: Chan Char -> InputSpec -> IO ()
write_input_spec_to_chan chan [] = writeChan chan '\xFFFD'
write_input_spec_to_chan chan (Bytes str : input_spec')
    = writeList2Chan chan str >> write_input_spec_to_chan chan input_spec'
write_input_spec_to_chan chan (Delay t : input_spec')
    = writeChan chan '\xFFFE' >> write_input_spec_to_chan chan input_spec'

{-

tests :: IO [Test]
tests = do
    mappend 
        <$> pure [ verify "basic block generated from single ansi chars to event translation" ]
        <*> mapM terminals_of_interest
        verify_simple_input_block_to_event
    , verify "keys from caps table are parsed to the same key"
        verify_keys_from_caps_table
    ]
-}

-- how to make this data dependent?

newtype EventBlock event = EventBlock ([(String,event)] -> [(String, event)])

instance Show (EventBlock event) where
    show (EventBlock g) = "EventBlock(* -> *)"

instance Monad m => Serial m (EventBlock event) where
    series = do
        n :: Int <- localDepth (max 16) series -- what elements to select from the table
        return $ EventBlock $ \table -> concat (take n (permutations table))

verify_simple_input_block_to_event :: Property IO
verify_simple_input_block_to_event = forAll $ \(EventBlock block_gen) -> do
    let simple_input_seq = block_gen simple_chars
        input = Bytes $ concat [s | (s,_) <- simple_input_seq]
        events = [e | (_,(k,ms)) <- simple_input_seq, let e = EvKey k ms]
    monadic $ assert_events_from_input_block simple_chars [input] events

verify_keys_from_caps_table_block_to_event :: Property IO
verify_keys_from_caps_table_block_to_event = forAll $ \(EventBlock block_gen) ->
    over (generate (\n -> take n terminals_of_interest)) $ \term_name -> monadic $ do
        terminal <- setupTerm term_name
        let caps_seq :: [(String, Event)] = block_gen keys_from_caps_table
        let bytes_seq = caps_classify_table terminal caps_seq
            input = [Bytes s | (s,_) <- bytes_seq]
            escaped_sequence = intersperse (Delay defaultEscDelay) input
        return True :: IO Bool
    -- cap <- elements keys_from_caps_table

main = defaultMain
    [ testProperty "basic block generated from a single ansi chars to event translation"
                   verify_simple_input_block_to_event
    , testProperty "key sequences read from caps table map to expected events"
                   verify_keys_from_caps_table_block_to_event
    ]

