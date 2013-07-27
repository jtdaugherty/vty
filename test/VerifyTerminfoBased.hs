{-# LANGUAGE ScopedTypeVariables #-}
{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyTerminfoBased where
import Verify

import Graphics.Vty
import Graphics.Vty.Terminal.TerminfoBased as TerminfoBased

import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Terminal

import qualified System.Console.Terminfo as Terminfo
import System.IO

tests :: IO [Test]
tests = concat <$> forM terminals_of_interest (\term_name -> do
    -- check if that terminfo exists
    putStrLn $ "testing end to end for terminal: " ++ term_name
    mti <- try $ Terminfo.setupTerm term_name
    case mti of
        Left (_ :: SomeException) -> return []
        Right _ -> return [ verify ("verify " ++ term_name ++ " could output a picture") 
                                   (smoke_test_term term_name)
                          ]
    )

smoke_test_term :: String -> Image -> Property
smoke_test_term term_name i = liftIOResult $ do
    null_out <- openFile "/dev/null" WriteMode
    t <- TerminfoBased.reserve_terminal term_name null_out
    putStrLn $ "context color count: " ++ show (context_color_count t)
    reserve_display t
    dc <- display_context t (DisplayRegion 100 100)
    -- always show the cursor to produce tests for terminals with no cursor support.
    let pic = (pic_for_image i) { pic_cursor = Cursor 0 0 }
    output_picture dc pic
    release_display t
    release_terminal t
    return succeeded

