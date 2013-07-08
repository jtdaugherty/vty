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

import qualified System.Console.Terminfo as Terminfo
import System.IO

-- A list of terminals that should be supported.
-- This started with a list of terminals ubuntu supported. Then those terminals that really could
-- not be supported were removed.
terminals_of_interest = 
    [ "vt100"
    , "vt220"
    , "vt102"
    , "xterm-r5"
    , "xterm-xfree86"
    , "xterm-r6"
    , "xterm-256color"
    , "xterm-vt220"
    , "xterm-debian"
    , "xterm-mono"
    , "xterm-color"
    , "xterm"
    , "mach"
    , "mach-bold"
    , "mach-color"
    , "linux"
    , "ansi"
    , "hurd"
    , "Eterm"
    , "pcansi"
    , "screen-256color"
    , "screen-bce"
    , "screen-s"
    , "screen-w"
    , "screen"
    , "screen-256color-bce"
    , "sun"
    , "rxvt"
    , "rxvt-unicode"
    , "rxvt-basic"
    , "cygwin"
    ]

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

smoke_test_term :: String -> SingleAttrSingleSpanStack -> Property
smoke_test_term term_name (SingleAttrSingleSpanStack i _ _ _) = liftIOResult $ do
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

