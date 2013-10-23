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

import Test.Framework.Providers.SmallCheck
import Test.Framework
import Test.SmallCheck
import Test.SmallCheck.Series

-- processing a block of 16 chars is the largest I can do without taking too long to run the test.
max_bytes_size = 16

data InputEvent
    = Bytes String  -- | input sequence encoded as a string. Regardless, the input is read a byte at a time.
    | Delay Int     -- | millisecond delay
    deriving Show

type InputSpec = [InputEvent]

type ExpectedSpec = [Event]

exec_input_spec :: InputSpec -> Handle -> IO ()
exec_input_spec input out_handle = forM_ input f
    where
        f (Bytes str) = hPutStr out_handle str >> hFlush out_handle
        f (Delay t) = threadDelay (t*1000)

assert_events_from_input_block :: ClassifyTable -> InputSpec -> ExpectedSpec -> IO Bool
assert_events_from_input_block table input_spec expected_events = do
    print input_spec
    let classifier = classify table
    input <- newChan
    output <- newChan
    read_complete <- newEmptyMVar
    write_complete <- newEmptyMVar
    _ <- forkIO $ write_input_spec_to_chan input input_spec
                  `finally` putMVar write_complete ()
    _ <- forkIO $ inputToEventThread classifier input output
                  `finally` putMVar read_complete ()
    () <- takeMVar write_complete
    () <- takeMVar read_complete
    -- TODO: use STM channel
    -- assures the next line does not block.
    writeList2Chan output $ replicate (length expected_events) undefined
    out_events <- take (length expected_events) <$> getChanContents output
    return $ out_events == expected_events

write_input_spec_to_chan :: Chan Char -> InputSpec -> IO ()
write_input_spec_to_chan chan [] = do
    writeChan chan '\xFFFD'
write_input_spec_to_chan chan (Bytes str : input_spec') = do
    writeList2Chan chan str
    write_input_spec_to_chan chan input_spec'
write_input_spec_to_chan chan (Delay _t : input_spec') = do
    writeChan chan '\xFFFE'
    write_input_spec_to_chan chan input_spec'

newtype EventBlock event = EventBlock ([(String,event)] -> [(String, event)])

instance Show (EventBlock event) where
    show (EventBlock g) = "EventBlock(*->*)"

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
        let table = caps_classify_table terminal keys_from_caps_table
            input_seq :: [(String, Event)] = block_gen table
            input_bytes = [Bytes s | (s,_) <- input_seq]
            input = intersperse (Delay defaultEscDelay) input_bytes
            events = [e | (_,e) <- input_seq]
        assert_events_from_input_block (map_to_legacy_table table) input events

main = defaultMain
    [ testProperty "basic block generated from a single ansi chars to event translation"
                   verify_simple_input_block_to_event
    , testProperty "key sequences read from caps table map to expected events"
                   verify_keys_from_caps_table_block_to_event
    ]

