{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright 2009-2010 Corey O'Connor
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Vty.Input ( Key(..)
                          , Modifier(..)
                          , Button(..)
                          , Event(..)
                          , initTermInput
                          )
    where

import Graphics.Vty.Input.Data

import Data.Char
import Data.Maybe ( mapMaybe
                  )
import Data.List( inits )
import Data.Word
import qualified Data.Map as M( fromList, lookup )
import qualified Data.Set as S( fromList, member )

import Codec.Binary.UTF8.Generic (decode)

import Control.Monad (when)
import Control.Concurrent
import Control.Exception

import System.Console.Terminfo

import System.Posix.Signals.Exts
import System.Posix.Terminal
import System.Posix.IO ( stdInput
                        ,fdReadBuf
                        ,setFdOption
                        ,FdOption(..)
                       )

import Foreign ( alloca, poke, peek, Ptr )

-- | Set up the terminal for input.  Returns a function which reads key
-- events, and a function for shutting down the terminal access.
initTermInput :: Int -> Terminal -> IO (IO Event, IO ())
initTermInput escDelay terminal = do
  eventChannel <- newChan
  inputChannel <- newChan
  hadInput <- newEmptyMVar
  oattr <- getTerminalAttributes stdInput
  let nattr = foldl withoutMode oattr [ StartStopOutput, KeyboardInterrupts
                                      , EnableEcho, ProcessInput, ExtendedFunctions
                                      ]
  setTerminalAttributes stdInput nattr Immediately
  set_term_timing
  let inputToEventThread :: IO ()
      inputToEventThread = loop []
          where loop kb = case (classify kb) of
                              Prefix       -> do c <- readChan inputChannel
                                                 loop (kb ++ [c])
                              Invalid      -> do c <- readChan inputChannel
                                                 loop [c]
                              MisPfx k m s -> writeChan eventChannel (EvKey k m) >> loop s
                              Valid k m    -> writeChan eventChannel (EvKey k m) >> loop ""

      finishAtomicInput = writeChan inputChannel '\xFFFE'

      inputThread :: IO ()
      inputThread = do
        _ <- alloca $ \(input_buffer :: Ptr Word8) -> do
            let loop = do
                    setFdOption stdInput NonBlockingRead False
                    threadWaitRead stdInput
                    setFdOption stdInput NonBlockingRead True
                    _ <- try readAll :: IO (Either IOException ())
                    when (escDelay == 0) finishAtomicInput
                    loop
                readAll = do
                    poke input_buffer 0
                    bytes_read <- fdReadBuf stdInput input_buffer 1
                    input_char <- fmap (chr . fromIntegral) $ peek input_buffer
                    when (bytes_read > 0) $ do
                        _ <- tryPutMVar hadInput () -- signal input
                        writeChan inputChannel input_char
                        readAll
            loop
        return ()

      -- | If there is no input for some time, this thread puts '\xFFFE' in the
      -- inputChannel.
      noInputThread :: IO ()
      noInputThread = when (escDelay > 0) loop
            where loop = do
                    takeMVar hadInput -- wait for some input
                    threadDelay escDelay -- microseconds
                    hadNoInput <- isEmptyMVar hadInput -- no input yet?
                    -- TODO(corey): there is a race between here and the inputThread.
                    when hadNoInput $ do
                        finishAtomicInput
                    loop


      compile :: [[([Char],(Key,[Modifier]))]] -> [Char] -> KClass
      compile lst = cl' where
          lst' = concat lst
          pfx = S.fromList $ concatMap (init . inits . fst) $ lst'
          mlst = M.fromList lst'
          cl' str = case S.member str pfx of
                      True -> Prefix
                      False -> case M.lookup str mlst of
                                 Just (k,m) -> Valid k m
                                 Nothing -> case head $ mapMaybe (\s -> (,) s `fmap` M.lookup s mlst) $ init $ inits str of
                                              (s,(k,m)) -> MisPfx k m (drop (length s) str)

      -- ANSI specific bits


      classify, classifyTab :: [Char] -> KClass

      -- As soon as
      classify "\xFFFE" = Invalid
      classify s@(c:_) | ord c >= 0xC2 =
          if utf8Length (ord c) > length s then Prefix else classifyUtf8 s -- beginning of an utf8 sequence
      classify other = classifyTab other

      classifyUtf8 s = case decode ((map (fromIntegral . ord) s) :: [Word8]) of
          Just (unicodeChar, _) -> Valid (KASCII unicodeChar) []
          _ -> Invalid -- something bad happened; just ignore and continue.

      classifyTab = compile (caps_classify_table : ansi_classify_table)

      caps_classify_table
        = map_to_legacy_table [(x,y) | (Just x,y) <- map (first (getCapability terminal . tiGetStr)) $ caps_table]

  eventThreadId <- forkIO $ inputToEventThread
  inputThreadId <- forkIO $ inputThread
  noInputThreadId <- forkIO $ noInputThread
  let pokeIO = (Catch $ do let e = error "(getsize in input layer)"
                           setTerminalAttributes stdInput nattr Immediately
                           writeChan eventChannel (EvResize e e))
  _ <- installHandler windowChange pokeIO Nothing
  _ <- installHandler continueProcess pokeIO Nothing
  -- TODO(corey): killThread is a bit risky for my tastes.
  let uninit = do killThread eventThreadId
                  killThread inputThreadId
                  killThread noInputThreadId
                  _ <- installHandler windowChange Ignore Nothing
                  _ <- installHandler continueProcess Ignore Nothing
                  setTerminalAttributes stdInput oattr Immediately
  return (readChan eventChannel, uninit)

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

utf8Length :: (Num t, Ord a, Num a) => a -> t
utf8Length c
    | c < 0x80 = 1
    | c < 0xE0 = 2
    | c < 0xF0 = 3
    | otherwise = 4

foreign import ccall "vty_set_term_timing" set_term_timing :: IO ()
