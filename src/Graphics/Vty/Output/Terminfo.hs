{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Graphics.Vty.Output.Terminfo where

import Graphics.Vty.Prelude

import Graphics.Vty.Config

import Graphics.Vty.Output.Interface

import Graphics.Vty.Output.Terminfo.Base as Base
import Graphics.Vty.Output.Terminfo.XTermColor as XTermColor

import Data.Default (def)
import Data.List (isPrefixOf)
import Data.Monoid ((<>))

import System.Posix.Env (getEnv)
import System.Posix.IO (stdOutput)

-- | Returns a `Output` for the terminal specified in `Config`
--
-- The specific Output implementation used is hidden from the API user. All terminal implementations
-- are assumed to perform more, or less, the same. Currently, all implementations use terminfo for at
-- least some terminal specific information.
--
-- Specifics about it being based on terminfo are hidden from the API user. If a terminal
-- implementation is developed for a terminal without terminfo support then Vty should work as
-- expected on that terminal.
--
-- Selection of a terminal is done as follows:
--
--      * If TERM == xterm use XTermColor.
--      * for any other TERM value TerminfoBased is used.
--
outputForConfig :: Config -> IO Output
outputForConfig Config{ outputFd = Just fd, termName = Just termName, .. } = do
    t <- if "xterm" `isPrefixOf` termName
        then XTermColor.reserveTerminal termName fd
        -- Not an xterm-like terminal. try for generic terminfo.
        else Base.reserveTerminal termName fd
    return t
outputForConfig config = (<> config) <$> defaultTerminfoOutputConfig >>= outputForConfig

defaultTerminfoOutputConfig :: IO Config
defaultTerminfoOutputConfig = do
  termName <- getEnv "TERM"
  when (termName == Nothing) $
    fail "vty: Terminfo output requires the TERM environment variable to be set."
  return $ def { outputFd = Just stdOutput
               , termName = termName
               }
