{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyUsingMockInput where
import Verify hiding (classify)

import Verify.Graphics.Vty.Terminal

import Graphics.Vty hiding (resize)
import Graphics.Vty.Input
import Graphics.Vty.Input.Data
import Graphics.Vty.Input.Internal

import Control.Concurrent
import Control.Exception
import Control.Monad

import System.Posix.Env
import System.IO

import Test.QuickCheck.Assertions
import Test.QuickCheck.Property ( morallyDubiousIOProperty )

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

assert_events_from_IO_reads :: InputSpec -> ExpectedSpec -> IO Result
assert_events_from_IO_reads input events = undefined

assert_events_from_input_block :: ClassifyTable -> InputSpec -> ExpectedSpec -> IO Result
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
    return $ out_events ?== expected_events

write_input_spec_to_chan :: Chan Char -> InputSpec -> IO ()
write_input_spec_to_chan chan [] = writeChan chan '\xFFFD'
write_input_spec_to_chan chan (Bytes str : input_spec')
    = writeList2Chan chan str >> write_input_spec_to_chan chan input_spec'
write_input_spec_to_chan chan (Delay t : input_spec')
    = writeChan chan '\xFFFE' >> write_input_spec_to_chan chan input_spec'

verify_simple_input_block_to_event :: Property
verify_simple_input_block_to_event = do
    -- Ouch! 16 is as high as this can go without taking far too long. :-\
    Positive block_length <- resize 16 $ arbitrary
    block_event_pairs <- vectorOf block_length $ elements $ simple_chars
    let input = [Bytes $ concatMap fst block_event_pairs]
        events = map (\(k,ms) -> EvKey k ms) $ map snd block_event_pairs
    morallyDubiousIOProperty $ assert_events_from_input_block simple_chars input events

verify_keys_from_caps_table :: String -> Result
verify_keys_from_caps_table term_name = succeeded

tests :: IO [Test]
tests = return
    [ verify "basic block generated from single ansi chars to event translation"
        verify_simple_input_block_to_event
    , verify "keys from caps table are parsed to the same key"
        verify_keys_from_caps_table
    ]

