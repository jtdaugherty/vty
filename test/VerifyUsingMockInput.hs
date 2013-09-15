{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyUsingMockInput where
import Verify

import Graphics.Vty

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

tests :: IO [Test]
tests = return []

