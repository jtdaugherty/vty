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
import Graphics.Vty.Input.Loop
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
maxBlockSize :: Int
maxBlockSize = 16

maxTableSize :: Int
maxTableSize = 28

forEachOf :: (Show a, Testable m b) => [a] -> (a -> b) -> Property m
forEachOf l = over (generate (\n -> take n l))

data InputEvent
    = Bytes String  -- | input sequence encoded as a string. Regardless, the input is read a byte at a time.
    | Delay Int     -- | microsecond delay
    deriving Show

type InputSpec = [InputEvent]

type ExpectedSpec = [Event]

synthesizeInput :: InputSpec -> Fd -> IO ()
synthesizeInput input outHandle = forM_ input f >> (void $ fdWrite outHandle "\xFFFD")
    where
        f (Bytes str) = void $ fdWrite outHandle str
        f (Delay t) = threadDelay t

minDetectableDelay :: Int
minDetectableDelay = 4000

minTimout :: Int
minTimout = 4000000

testKeyDelay :: Int
testKeyDelay = minDetectableDelay * 4

testEscSampleDelay :: Int
testEscSampleDelay = minDetectableDelay * 2

genEventsUsingIoActions :: Int -> IO () -> IO () -> IO ()
genEventsUsingIoActions maxDuration inputAction outputAction = do
    let maxDuration' = max minTimout maxDuration
    readComplete <- newEmptyMVar
    writeComplete <- newEmptyMVar
    _ <- forkOS $ inputAction `finally` putMVar writeComplete ()
    _ <- forkOS $ outputAction `finally` putMVar readComplete ()
    Just () <- timeout maxDuration' $ takeMVar writeComplete
    Just () <- timeout maxDuration' $ takeMVar readComplete
    return ()

compareEvents :: (Show a1, Show a, Eq a1) => a -> [a1] -> [a1] -> IO Bool
compareEvents inputSpec expectedEvents outEvents = compareEvents' expectedEvents outEvents
    where
        compareEvents' [] []         = return True
        compareEvents' [] outEvents' = do
            printf "extra events %s\n" (show outEvents') :: IO ()
            return False
        compareEvents' expectedEvents' [] = do
            printf "events %s were not produced for input %s\n" (show expectedEvents') (show inputSpec) :: IO ()
            printf "expected events %s\n" (show expectedEvents) :: IO ()
            printf "received events %s\n" (show outEvents) :: IO ()
            return False
        compareEvents' (e : expectedEvents') (o : outEvents')
            | e == o    = compareEvents' expectedEvents' outEvents'
            | otherwise = do
                printf "%s expected not %s for input %s\n" (show e) (show o) (show inputSpec) :: IO ()
                printf "expected events %s\n" (show expectedEvents) :: IO ()
                printf "received events %s\n" (show outEvents) :: IO ()
                return False

assertEventsFromSynInput :: ClassifyTable -> InputSpec -> ExpectedSpec -> IO Bool
assertEventsFromSynInput table inputSpec expectedEvents = do
    let maxDuration = sum [t | Delay t <- inputSpec] + minDetectableDelay
        eventCount = length expectedEvents
    (writeFd, readFd) <- openPseudoTerminal
    (setTermAttr,_) <- attributeControl readFd
    setTermAttr
    input <- initInputForFd def table readFd
    eventsRef <- newIORef []
    let writeWaitClose = do
            synthesizeInput inputSpec writeFd
            threadDelay minDetectableDelay
            shutdownInput input
            threadDelay minDetectableDelay
            closeFd writeFd
            closeFd readFd
    -- drain output pipe
    let readEvents = readLoop eventCount
        readLoop 0 = return ()
        readLoop n = do
            e <- readChan $ input^.eventChannel
            modifyIORef eventsRef ((:) e)
            readLoop (n - 1)
    genEventsUsingIoActions maxDuration writeWaitClose readEvents
    outEvents <- reverse <$> readIORef eventsRef
    compareEvents inputSpec expectedEvents outEvents

newtype InputBlocksUsingTable event
    = InputBlocksUsingTable ([(String,event)] -> [(String, event)])

instance Show (InputBlocksUsingTable event) where
    show (InputBlocksUsingTable _g) = "InputBlocksUsingTable"

instance Monad m => Serial m (InputBlocksUsingTable event) where
    series = do
        n :: Int <- localDepth (const maxTableSize) series
        return $ InputBlocksUsingTable $ \table -> concat (take n (selections table))
        where
            selections []     = []
            selections (x:xs) = let z = selections xs in [x] : (z ++ map ((:) x) z)

verifyVisibleSynInputToEvent :: Property IO
verifyVisibleSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table    = visibleChars
        inputSeq = gen table
        events   = map snd inputSeq
        keydowns = map (Bytes . fst) inputSeq
        input    = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
    assertEventsFromSynInput universalTable input events

verifyCapsSynInputToEvent :: Property IO
verifyCapsSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminalsOfInterest $ \termName -> monadic $ do
        term <- setupTerm termName
        let table         = capsClassifyTable term keysFromCapsTable
            inputSeq     = gen table
            events        = map snd inputSeq
            keydowns      = map (Bytes . fst) inputSeq
            input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
        assertEventsFromSynInput table input events

verifySpecialSynInputToEvent :: Property IO
verifySpecialSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table         = specialSupportKeys
        inputSeq     = gen table
        events        = map snd inputSeq
        keydowns      = map (Bytes . fst) inputSeq
        input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
    assertEventsFromSynInput universalTable input events

verifyFullSynInputToEvent :: Property IO
verifyFullSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminalsOfInterest $ \termName -> monadic $ do
        term <- setupTerm termName
        let table         = classifyTableForTerm termName term
            inputSeq     = gen table
            events        = map snd inputSeq
            keydowns      = map (Bytes . fst) inputSeq
            input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
        assertEventsFromSynInput table input events

verifyFullSynInputToEvent_2x :: Property IO
verifyFullSynInputToEvent_2x = forAll $ \(InputBlocksUsingTable gen) ->
    forEachOf terminalsOfInterest $ \termName -> monadic $ do
        term <- setupTerm termName
        let table         = classifyTableForTerm termName term
            inputSeq     = gen table
            events        = concatMap ((\s -> [s,s]) . snd) inputSeq
            keydowns      = map (Bytes . (\s -> s ++ s) . fst) inputSeq
            input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
        assertEventsFromSynInput table input events

main :: IO ()
main = defaultMain
    [ testProperty "synthesized typing of single visible chars translates to expected events"
        verifyVisibleSynInputToEvent
    , testProperty "synthesized typing of keys from capabilities tables translates to expected events"
        verifyCapsSynInputToEvent
    , testProperty "synthesized typing of hard coded special keys translates to expected events"
        verifySpecialSynInputToEvent
    , testProperty "synthesized typing of any key in the table translates to its paired event"
        verifyFullSynInputToEvent
    , testProperty "synthesized typing of 2x any key in the table translates to 2x paired event"
        verifyFullSynInputToEvent_2x
    ]

