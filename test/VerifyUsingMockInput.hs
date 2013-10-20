{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module Main where

import Verify.Graphics.Vty.Terminal

import Graphics.Vty hiding (resize)
import Graphics.Vty.Input
import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad

import System.Posix.Env
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

verify_keys_from_caps_table :: String -> Property
verify_keys_from_caps_table = do
    term_name <- elements terminals_of_interest
    cap <- elements keys_from_caps_table
    

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

data SimpleEvent = SimpleEvent (String, (Key, [Modifier]))
    deriving (Show)

instance Monad m => Serial m SimpleEvent where
    series = generate (\n -> map SimpleEvent $ take n simple_chars)

verify_simple_input_block_to_event :: Property IO
verify_simple_input_block_to_event = forAll $ \input_spec -> do
    let input = Bytes $ concat [s | SimpleEvent (s,_) <- input_spec]
        events = [e | SimpleEvent (_,(k,ms)) <- input_spec, let e = EvKey k ms]
    monadic $ assert_events_from_input_block simple_chars [input] events

main = defaultMain
    [ testProperty "basic block generated from a single ansi chars to event translation"
                   verify_simple_input_block_to_event
    ]

