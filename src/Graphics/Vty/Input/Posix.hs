{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#ifndef MIN_VERSION_base
#defined MIN_VERSION_base(x,y,z) 1
#endif

module Graphics.Vty.Input.Posix where

import Graphics.Vty.Config
import Graphics.Vty.Input.Events
import Graphics.Vty.Input.Interface
import Graphics.Vty.Input.Posix.Loop
import Graphics.Vty.Input.Terminfo

import Control.Concurrent.STM
import Control.Lens
import Control.Monad (when)

import Data.Default
#if !(MIN_VERSION_base(4,8,0))
import Data.Functor ((<$>))
import Data.Monoid
#else
import Data.Monoid ((<>))
#endif

import qualified System.Console.Terminfo as Terminfo
import System.Posix.Env (getEnv)
import System.Posix.IO (stdInput)
import System.Posix.Signals.Exts

-- | Set up the terminal with file descriptor `inputFd` for input.  Returns an 'Input'.
--
-- The table used to determine the 'Events' to produce for the input bytes comes from
-- 'classifyMapForTerm'. Which is then overridden by the the applicable entries from
-- 'inputMap'.
--
-- The terminal device is configured with the attributes:
--
-- * IXON disabled
--      - disables software flow control on outgoing data. This stops the process from being
--        suspended if the output terminal cannot keep up. I presume this has little effect these
--        days. I hope this means that output will be buffered if the terminal cannot keep up. In the
--        old days the output might of been dropped?
--
-- "raw" mode is used for input.
--
-- * ISIG disabled
--      - enables keyboard combinations that result in signals. TODO: should probably be a dynamic
--      option.
--
-- * ECHO disabled
--      - input is not echod to the output. TODO: should be a dynamic option.
--
-- * ICANON disabled
--      - canonical mode (line mode) input is not used. TODO: should be a dynamic option.
--
-- * IEXTEN disabled
--      - extended functions are disabled. TODO: I don't know what those are.
--
inputForConfig :: Config -> IO Input
inputForConfig config@Config{ termName = Just termName
                            , inputFd = Just termFd
                            , vmin = Just _
                            , vtime = Just _
                            , .. } = do
    terminal <- Terminfo.setupTerm termName
    let inputOverrides = [(s,e) | (t,s,e) <- inputMap, t == Nothing || t == Just termName]
        activeInputMap = classifyMapForTerm termName terminal `mappend` inputOverrides
    (setAttrs,unsetAttrs) <- attributeControl termFd
    setAttrs
    input <- initInput config activeInputMap
    let pokeIO = Catch $ do
            let e = error "vty internal failure: this value should not propagate to users"
            setAttrs
            atomically $ writeTChan (input^.eventChannel) (EvResize e e)
    _ <- installHandler windowChange pokeIO Nothing
    _ <- installHandler continueProcess pokeIO Nothing
    return $ input
        { shutdownInput = do
            shutdownInput input
            _ <- installHandler windowChange Ignore Nothing
            _ <- installHandler continueProcess Ignore Nothing
            unsetAttrs
        }
inputForConfig config = (<> config) <$> defaultTerminfoInputConfig >>= inputForConfig

defaultTerminfoInputConfig :: IO Config
defaultTerminfoInputConfig = do
  termName <- getEnv "TERM"
  when (termName == Nothing) $
    fail "vty: Terminfo input requires the TERM environment variable to be set."
  return $ def { vmin     = Just 1
               , vtime    = Just 100
               , inputFd  = Just stdInput
               , termName = termName
               }
