{- We setup the environment to envoke certain terminals of interest.
 - This assumes appropriate definitions exist in the current environment for the terminals of
 - interest.
 -}
module VerifyOutput where
import Verify

import Graphics.Vty

import Verify.Graphics.Vty.Image
import Verify.Graphics.Vty.Output

import Control.Monad

import qualified System.Console.Terminfo as Terminfo
import System.Posix.Env
import System.IO

tests :: IO [Test]
tests = concat <$> forM terminalsOfInterest (\termName -> do
    -- check if that terminfo exists
    putStrLn $ "testing end to end for terminal: " ++ termName
    mti <- try $ Terminfo.setupTerm termName
    case mti of
        Left (_ :: SomeException) -> return []
        Right _ -> return [ verify ("verify " ++ termName ++ " could output a picture")
                                   (smokeTestTermNonMac termName)
                          -- this is excessive.
                          , verify ("verify " ++ termName ++ " could output a picture on a Mac.")
                                   (smokeTestTermMac termName)
                          ]
    )

smokeTestTermNonMac :: String -> Image -> Property
smokeTestTermNonMac termName i = liftIOResult $ do
    -- unset the TERM_PROGRAM environment variable if set.
    -- Required to execute regression test for #42 on a mac
    unsetEnv "TERM_PROGRAM"
    smokeTestTerm termName i

smokeTestTermMac :: String -> Image -> Property
smokeTestTermMac termName i = liftIOResult $ do
    setEnv "TERM_PROGRAM" "Apple_Terminal" True
    smokeTestTerm termName i

smokeTestTerm :: String -> Image -> IO Result
smokeTestTerm termName i = do
    nullOut <- openFile "/dev/null" WriteMode
    t <- outputForNameAndIO termName nullOut
    putStrLn $ "context color count: " ++ show (contextColorCount t)
    reserveDisplay t
    dc <- displayContext t (100,100)
    -- always show the cursor to produce tests for terminals with no cursor support.
    let pic = (picForImage i) { picCursor = Cursor 0 0 }
    outputPicture dc pic
    setCursorPos t 0 0
    when (supportsCursorVisibility t) $ do
        hideCursor t
        showCursor t
    releaseDisplay t
    releaseTerminal t
    return succeeded

