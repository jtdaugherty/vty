-- Copyright 2009-2010 Corey O'Connor
module Graphics.Vty.Output.XTermColor ( reserve_terminal )
    where

import Graphics.Vty.Output.Interface
import qualified Graphics.Vty.Output.TerminfoBased as TerminfoBased

import Control.Applicative
import Control.Monad.Trans

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.IO

-- | Initialize the display to UTF-8. 
reserve_terminal :: ( Applicative m, MonadIO m ) => String -> Handle -> m Output
reserve_terminal variant out_handle = liftIO $ do
    let flushed_put str = do
            hPutStr out_handle str
            hFlush out_handle
    -- If the terminal variant is xterm-color use xterm instead since, more often than not,
    -- xterm-color is broken.
    let variant' = if variant == "xterm-color" then "xterm" else variant
    flushed_put set_utf8_char_set
    t <- TerminfoBased.reserve_terminal variant' out_handle
    let t' = t
             { terminal_ID = terminal_ID t ++ " (xterm-color)"
             , release_terminal = do
                 liftIO $ flushed_put set_default_char_set
                 release_terminal t
             , mk_display_context = \t_actual r -> do
                dc <- mk_display_context t t_actual r
                return $ dc { inline_hack = xterm_inline_hack t' }
             }
    return t'

-- | These sequences set xterm based terminals to UTF-8 output.
--
-- \todo I don't know of a terminfo cap that is equivalent to this.
set_utf8_char_set, set_default_char_set :: String
set_utf8_char_set = "\ESC%G"
set_default_char_set = "\ESC%@"

-- | I think xterm is broken: Reseting the background color as the first bytes serialized on a
-- new line does not effect the background color xterm uses to clear the line. Which is used
-- *after* the next newline.
xterm_inline_hack :: Output -> IO ()
xterm_inline_hack t = do
    let s_utf8 = T.encodeUtf8 $ T.pack "\ESC[K"
    send_to_terminal t (utf8_text_required_bytes s_utf8)
                       (serialize_utf8_text s_utf8)

