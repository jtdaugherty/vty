--  | Output interface.
--
--  Access to the current terminal or a specific terminal device.
--
--  See also:
--
--  1. "Graphics.Vty.Output": This instantiates an abtract interface to the terminal interface based
--  on the TERM and COLORTERM environment variables. 
--  
--  2. "Graphics.Vty.Output.Interface": Defines the generic interface all terminals need to implement.
--
--  3. "Graphics.Vty.Output.TerminfoBased": Defines a terminal instance that uses terminfo for all
--  control strings.  No attempt is made to change the character set to UTF-8 for these terminals.
--  I don't know a way to reliably determine if that is required or how to do so.
--
--  4. "Graphics.Vty.Output.XTermColor": This module contains an interface suitable for xterm-like
--  terminals. These are the terminals where TERM == xterm. This does use terminfo for as many
--  control codes as possible. 
module Graphics.Vty.Output ( module Graphics.Vty.Output
                           , Output(..) -- \todo hide constructors
                           , AssumedState(..)
                           , DisplayContext(..) -- \todo hide constructors
                           , outputPicture
                           , displayContext
                           )
    where


import Graphics.Vty.Prelude

import Graphics.Vty.Output.Interface
import Graphics.Vty.Output.MacOSX as MacOSX
import Graphics.Vty.Output.XTermColor as XTermColor
import Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Blaze.ByteString.Builder (writeToByteString)

import Control.Exception ( SomeException, try )
import Control.Monad.Trans

import Data.List ( isPrefixOf )

import GHC.IO.Handle

import System.Environment
import System.IO

-- | Returns a `Output` for the current terminal as determined by TERM.
--
-- The specific Output implementation used is hidden from the API user. All terminal implementations
-- are assumed to perform more, or less, the same. Currently all implementations use terminfo for at
-- least some terminal specific information. This is why platforms without terminfo are not
-- supported. However, as mentioned before, any specifics about it being based on terminfo are
-- hidden from the API user.  If a terminal implementation is developed for a terminal for a
-- platform without terminfo support then Vty should work as expected on that terminal.
--
-- Selection of a terminal is done as follows:
--
--      * If TERM == xterm
--          then the terminal might be one of the Mac OS X .app terminals. Check if that might be
--          the case and use MacOSX if so.
--          otherwise use XTermColor.
--
--      * for any other TERM value TerminfoBased is used.
--
-- To differentiate between Mac OS X terminals this uses the TERM_PROGRAM environment variable.
-- However, an xterm started by Terminal or iTerm *also* has TERM_PROGRAM defined since the
-- environment variable is not reset/cleared by xterm. However a Terminal.app or iTerm.app started
-- from an xterm under X11 on mac os x will likely be done via open. Since this does not propogate
-- environment variables (I think?) this assumes that XTERM_VERSION will never be set for a true
-- Terminal.app or iTerm.app session.
--
-- The file descriptor used for output will a be a duplicate of the current stdout file descriptor.
--
-- \todo add an implementation for windows that does not depend on terminfo. Should be installable
-- with only what is provided in the haskell platform. Use ansi-terminal
outputForCurrentTerminal :: ( Applicative m, MonadIO m ) => Config -> m Output
outputForCurrentTerminal _config = do
    termType <- liftIO $ getEnv "TERM"
    outHandle <- liftIO $ hDuplicate stdout
    outputForNameAndIO termType outHandle

-- | gives an output method structure for a terminal with the given name and the given 'Handle'.
outputForNameAndIO :: (Applicative m, MonadIO m) => String -> Handle -> m Output
outputForNameAndIO termType outHandle = do
    t <- if "xterm" `isPrefixOf` termType
        then do
            maybeTerminalApp <- mGetEnv "TERM_PROGRAM"
            case maybeTerminalApp of
                Nothing
                    -> XTermColor.reserveTerminal termType outHandle
                Just v | v == "Apple_Terminal" || v == "iTerm.app" 
                    -> do
                        maybeXterm <- mGetEnv "XTERM_VERSION"
                        case maybeXterm of
                            Nothing -> MacOSX.reserveTerminal v outHandle
                            Just _  -> XTermColor.reserveTerminal termType outHandle
                -- Assume any other terminal that sets TERM_PROGRAM to not be an OS X terminal.app
                -- like terminal?
                _   -> XTermColor.reserveTerminal termType outHandle
        -- Not an xterm-like terminal. try for generic terminfo.
        else TerminfoBased.reserveTerminal termType outHandle
    return t
    where
        mGetEnv var = do
            mv <- liftIO $ try $ getEnv var
            case mv of
                Left (_e :: SomeException)  -> return $ Nothing
                Right v -> return $ Just v

-- | Sets the cursor position to the given output column and row. 
--
-- This is not necessarially the same as the character position with the same coordinates.
-- Characters can be a variable number of columns in width.
--
-- Currently, the only way to set the cursor position to a given character coordinate is to specify
-- the coordinate in the Picture instance provided to outputPicture or refresh.
setCursorPos :: MonadIO m => Output -> Int -> Int -> m ()
setCursorPos t x y = do
    bounds <- displayBounds t
    when (x >= 0 && x < regionWidth bounds && y >= 0 && y < regionHeight bounds) $ do
        dc <- displayContext t bounds
        liftIO $ outputByteBuffer t $ writeToByteString $ writeMoveCursor dc x y

-- | Hides the cursor
hideCursor :: MonadIO m => Output -> m ()
hideCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    liftIO $ outputByteBuffer t $ writeToByteString $ writeHideCursor dc
    
-- | Shows the cursor
showCursor :: MonadIO m => Output -> m ()
showCursor t = do
    bounds <- displayBounds t
    dc <- displayContext t bounds
    liftIO $ outputByteBuffer t $ writeToByteString $ writeShowCursor dc

