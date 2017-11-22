-- | St output driver (https://st.suckless.org/). This uses the Terminfo
-- driver with some extensions for St.
module Graphics.Vty.Output.St
  ( reserveTerminal
  )
where

import Graphics.Vty.Output.Interface
import Graphics.Vty.Input.Mouse
import Graphics.Vty.Input.Focus
import qualified Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Blaze.ByteString.Builder (writeToByteString)
import Blaze.ByteString.Builder.Word (writeWord8)

import Control.Monad (void, when)
import Control.Monad.Trans
import Data.IORef

import System.Posix.IO (fdWrite)
import System.Posix.Types (Fd)
import System.Posix.Env (getEnv)

import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

-- | Construct an St output driver. Initialize the display to UTF-8.
reserveTerminal :: ( Applicative m, MonadIO m ) => String -> Fd -> m Output
reserveTerminal variant outFd = liftIO $ do
    let flushedPut = void . fdWrite outFd

    utf8a <- utf8Active
    when (not utf8a) $ flushedPut setUtf8CharSet
    t <- TerminfoBased.reserveTerminal variant outFd

    pasteModeStatus <- newIORef False

    let stSetMode t' m newStatus = do
          curStatus <- getModeStatus t' m
          when (newStatus /= curStatus) $
              case m of
                  BracketedPaste -> liftIO $ do
                      case newStatus of
                          True -> flushedPut enableBracketedPastes
                          False -> flushedPut disableBracketedPastes
                      writeIORef pasteModeStatus newStatus
                  _ -> return ()

        stGetMode BracketedPaste = liftIO $ readIORef pasteModeStatus
        stGetMode _ = return False

    let t' = t
             { terminalID = terminalID t ++ " (st)"
             , releaseTerminal = do
                 when (not utf8a) $ liftIO $ flushedPut setDefaultCharSet
                 setMode t' BracketedPaste False
                 releaseTerminal t
             , mkDisplayContext = mkDisplayContext t
             , supportsMode = const True
             , getModeStatus = stGetMode
             , setMode = stSetMode t'
             }
    return t'

utf8Active :: IO Bool
utf8Active = do
    let vars = ["LC_ALL", "LANG", "LC_CTYPE"]
    results <- catMaybes <$> mapM getEnv vars
    let matches = filter ("UTF8" `isInfixOf`) results <>
                  filter ("UTF-8" `isInfixOf`) results
    return $ not $ null matches

-- | Enable bracketed paste mode:
-- http://cirw.in/blog/bracketed-paste
enableBracketedPastes :: String
enableBracketedPastes = "\ESC[?2004h"

-- | Disable bracketed paste mode:
disableBracketedPastes :: String
disableBracketedPastes = "\ESC[?2004l"

-- | These sequences set xterm based terminals to UTF-8 output.
--
-- There is no known terminfo capability equivalent to this.
setUtf8CharSet, setDefaultCharSet :: String
setUtf8CharSet = "\ESC%G"
setDefaultCharSet = "\ESC%@"
