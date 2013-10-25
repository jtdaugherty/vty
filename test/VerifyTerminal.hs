{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyTerminal where
import Verify

import Graphics.Vty

import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Terminal

import Control.Monad

import qualified System.Console.Terminfo as Terminfo
import System.Posix.Env
import System.IO

tests :: IO [Test]
tests = concat <$> forM terminals_of_interest (\term_name -> do
    -- check if that terminfo exists
    putStrLn $ "testing end to end for terminal: " ++ term_name
    mti <- try $ Terminfo.setupTerm term_name
    case mti of
        Left (_ :: SomeException) -> return []
        Right _ -> return [ verify ("verify " ++ term_name ++ " could output a picture")
                                   (smoke_test_term_non_mac term_name)
                          -- this is excessive.
                          , verify ("verify " ++ term_name ++ " could output a picture on a Mac.")
                                   (smoke_test_term_mac term_name)
                          ]
    )

smoke_test_term_non_mac :: String -> Image -> Property
smoke_test_term_non_mac term_name i = liftIOResult $ do
    -- unset the TERM_PROGRAM environment variable if set.
    -- Required to execute regression test for #42 on a mac
    unsetEnv "TERM_PROGRAM"
    smoke_test_term term_name i

smoke_test_term_mac :: String -> Image -> Property
smoke_test_term_mac term_name i = liftIOResult $ do
    setEnv "TERM_PROGRAM" "Apple_Terminal" True
    smoke_test_term term_name i

smoke_test_term :: String -> Image -> IO Result
smoke_test_term term_name i = do
    null_out <- openFile "/dev/null" WriteMode
    t <- terminal_with_name_and_io term_name null_out
    putStrLn $ "context color count: " ++ show (context_color_count t)
    reserve_display t
    dc <- display_context t (DisplayRegion 100 100)
    -- always show the cursor to produce tests for terminals with no cursor support.
    let pic = (pic_for_image i) { pic_cursor = Cursor 0 0 }
    output_picture dc pic
    set_cursor_pos t 0 0
    when (supports_cursor_visibility t) $ do
        hide_cursor t
        show_cursor t
    release_display t
    release_terminal t
    return succeeded

