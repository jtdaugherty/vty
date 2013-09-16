{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyUsingMockInput where
import Verify

import Graphics.Vty
import Graphics.Vty.Input.Data

import Control.Concurrent
import Control.Monad

import System.Posix.Env
import System.IO

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

assert_events_from_input_block :: InputSpec -> ExpectedSpec -> Result
assert_events_from_input_block input events = undefined

verify_ansi_table_input_block_to_event :: Gen Result
verify_ansi_table_input_block_to_event = do
    Positive block_length <- arbitrary
    block_event_pairs <- vectorOf block_length $ elements $ concat ansi_classify_table
    let input = [Bytes $ concatMap fst block_event_pairs]
        events = map (\(k,ms) -> EvKey k ms) $ map snd block_event_pairs
    return $ assert_events_from_input_block input events

tests :: IO [Test]
tests = return
    [ verify "basic input block to event translation" verify_ansi_table_input_block_to_event
    ]

