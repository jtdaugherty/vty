-- Copyright Corey O'Connor
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | This provides a mock terminal implementation that is nice for testing. This transforms the
-- output operations to visible characters. Which is nice for some tests.
module Graphics.Vty.Output.Mock ( MockData, mock_terminal )
    where

import Graphics.Vty.Prelude

import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder.Word (writeWord8)

import Control.Monad.Trans

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

type MockData = IORef (UTF8.UTF8 BS.ByteString)

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
mock_terminal :: (Applicative m, MonadIO m) => DisplayRegion -> m (MockData, Output)
mock_terminal r = liftIO $ do
    out_ref <- newIORef undefined
    new_assumed_state_ref <- newIORef initial_assumed_state
    let t = Output
            { terminal_ID = "mock terminal"
            , release_terminal = return ()
            , reserve_display = return ()
            , release_display = return ()
            , display_bounds = return r
            , output_byte_buffer = \bytes -> do
                putStrLn $ "mock output_byte_buffer of " ++ show (BS.length bytes) ++ " bytes"
                writeIORef out_ref $ UTF8.fromRep bytes
            , context_color_count = 16
            , supports_cursor_visibility = True
            , assumed_state_ref = new_assumed_state_ref
            , mk_display_context = \t_actual r_actual -> return $ DisplayContext
                { context_region = r_actual
                , context_device = t_actual
                -- A cursor move is always visualized as the single character 'M'
                , write_move_cursor = \_x _y -> writeWord8 $ toEnum $ fromEnum 'M'
                -- Show cursor is always visualized as the single character 'S'
                , write_show_cursor =  writeWord8 $ toEnum $ fromEnum 'S'
                -- Hide cursor is always visualized as the single character 'H'
                , write_hide_cursor = writeWord8 $ toEnum $ fromEnum 'H'
                -- An attr change is always visualized as the single character 'A'
                , write_set_attr = \_fattr _diffs _attr -> writeWord8 $ toEnum $ fromEnum 'A'
                -- default attr is always visualized as the single character 'D'
                , write_default_attr = writeWord8 $ toEnum $ fromEnum 'D'
                -- row end is always visualized as the single character 'E'
                , write_row_end = writeWord8 $ toEnum $ fromEnum 'E'
                , inline_hack = return ()
                }
            }
    return (out_ref, t)

