-- Copyright Corey O'Connor
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | This provides a mock terminal implementation that is nice for testing. This transforms the
-- output operations to visible characters. Which is nice for some tests.
module Graphics.Vty.Output.Mock ( MockData, mockTerminal )
    where

import Graphics.Vty.Prelude

import Graphics.Vty.Output.Interface

import Control.Monad.Operational
import Control.Monad.Trans

import Data.IORef
import qualified Data.String.UTF8 as UTF8

type MockData = IORef String

-- | The mock display terminal produces a string representation of the requested picture.  There is
-- *not* an isomorphism between the string representation and the picture.  The string
-- representation is a simplification of the picture that is only useful in debugging VTY without
-- considering terminal specific issues.
--
-- The mock implementation is useful in manually determining if the sequence of terminal operations
-- matches the expected sequence. So requirement of the produced representation is simplicity in
-- parsing the text representation and determining how the picture was mapped to terminal
-- operations.
--
-- The string representation is a sequence of identifiers where each identifier is the name of an
-- operation in the algebra.
mockTerminal :: (Applicative m, MonadIO m) => DisplayRegion -> m (MockData, Output)
mockTerminal r = liftIO $ do
    outRef <- newIORef undefined
    newAssumedStateRef <- newIORef initialAssumedState
    let t = Output
            { terminalID = "mock terminal"
            , releaseTerminal = return ()
            , reserveDisplay = return ()
            , releaseDisplay = return ()
            , displayBounds = return r
            , contextColorCount = 16
            , supportsCursorVisibility = True
            , assumedStateRef = newAssumedStateRef
            , outputDisplayCommands =
                let out :: String -> IO ()
                    out str = modifyIORef outRef (++ str)
                    interpret :: DisplayCommands a -> IO a
                    interpret = eval . view
                    eval :: ProgramView DisplayCommand a -> IO a
                    eval (Return a) = return a
                    -- A cursor move is always visualized as the single character 'M'
                    eval (MoveCursor _ _ :>>= cmds') = out "M" >> interpret (cmds' ())
                    -- An attr change is always visualized as the single character 'A'
                    eval (SetAttr _ _ _  :>>= cmds') = out "A" >> interpret (cmds' ())
                    -- Show cursor is always visualized as the single character 'S'
                    eval (ShowCursor     :>>= cmds') = out "S" >> interpret (cmds' ())
                    -- Hide cursor is always visualized as the single character 'H'
                    eval (HideCursor     :>>= cmds') = out "H" >> interpret (cmds' ())
                    -- row end is always visualized as the single character 'E'
                    eval (DisplayRowEnd  :>>= cmds') = out "E" >> interpret (cmds' ())
                    -- default attr is always visualized as the single character 'D'
                    eval (DefaultAttr    :>>= cmds') = out "D" >> interpret (cmds' ())
                    -- UTF8 Text is represented as the input text converted to a String
                    eval (Utf8Text bs    :>>= cmds') =
                        let str = UTF8.toString $ UTF8.fromRep bs
                        in out str >> interpret (cmds' ())
                in \cmds -> liftIO $ writeIORef outRef "" >> interpret cmds
            }
    return (outRef, t)
