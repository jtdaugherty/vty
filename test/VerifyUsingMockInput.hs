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
max_block_size :: Int
max_block_size = 16

forEachOf :: (Show a, Testable m b) => [a] -> (a -> b) -> Property m
forEachOf l = over (generate (\n -> take n l))

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

newtype InputBlocksUsingTable event
    = InputBlocksUsingTable ([(String,event)] -> [(String, event)])

instance Show (InputBlocksUsingTable event) where
    show (InputBlocksUsingTable _g) = "InputBlocksUsingTable"

instance Monad m => Serial m (InputBlocksUsingTable event) where
    series = do
        n :: Int <- localDepth (max max_block_size) series -- what elements to select from the table
        return $ InputBlocksUsingTable $ \table -> concat (take n (permutations table))

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
            input         = intersperse (Delay defaultEscDelay) keydowns
        assert_events_from_input_block (map_to_legacy_table table) input events

main :: IO ()
main = defaultMain
    [ testProperty "basic block generated from a single ansi chars to event translation"
                   verify_simple_input_block_to_event
    , testProperty "key sequences read from caps table map to expected events"
                   verify_keys_from_caps_table_block_to_event
    ]

