-- Copyright Corey O'Connor
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Vty.Terminal.Mock ( mock_terminal )
    where

import Graphics.Vty.DisplayRegion
import Graphics.Vty.Terminal.Interface

import Control.Applicative
import Control.Monad.Trans

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.String.UTF8 as UTF8

import Foreign.Marshal.Array ( peekArray )
import Foreign.Ptr ( plusPtr )
import Foreign.Storable ( poke )

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
-- All terminals support the operations specified in the Terminal class defined in
-- Graphics.Vty.Terminal. As an instance of the Terminal class is also an instance of the Monad
-- class there exists a monoid that defines it's algebra. The string representation is a sequence of
-- identifiers where each identifier is the name of an operation in the algebra.
mock_terminal :: (Applicative m, MonadIO m) => DisplayRegion -> m (MockData, Terminal)
mock_terminal r = liftIO $ do
    out_ref <- newIORef undefined
    new_assumed_state_ref <- newIORef initial_assumed_state
    let t = Terminal
            { terminal_ID = "mock terminal"
            , release_terminal = return ()
            , reserve_display = return ()
            , release_display = return ()
            , display_bounds = return r
            , output_byte_buffer = \out_buffer buffer_size -> do
                putStrLn $ "mock output_byte_buffer of " ++ show buffer_size ++ " bytes"
                peekArray (fromEnum buffer_size) out_buffer 
                >>= return . UTF8.fromRep . BS.pack
                >>= writeIORef out_ref
            , context_color_count = 16
            , supports_cursor_visibility = True
            , assumed_state_ref = new_assumed_state_ref
            , display_context = \r_ -> return $ DisplayContext
                { context_region = r_
                , context_device = t
                -- A cursor move is always visualized as the single character 'M'
                , move_cursor_required_bytes = \_x _y -> 1
                , serialize_move_cursor = \_x _y ptr -> do
                    poke ptr (toEnum $ fromEnum 'M') 
                    return $ ptr `plusPtr` 1
                -- Show cursor is always visualized as the single character 'S'
                , show_cursor_required_bytes = 1
                , serialize_show_cursor = \ptr -> do
                    poke ptr (toEnum $ fromEnum 'S') 
                    return $ ptr `plusPtr` 1
                -- Hide cursor is always visualized as the single character 'H'
                , hide_cursor_required_bytes = 1
                , serialize_hide_cursor = \ptr -> do
                    poke ptr (toEnum $ fromEnum 'H') 
                    return $ ptr `plusPtr` 1
                -- An attr change is always visualized as the single character 'A'
                , attr_required_bytes = \_fattr _diffs _attr -> 1
                , serialize_set_attr = \_fattr _diffs _attr ptr -> do
                    poke ptr (toEnum $ fromEnum 'A')
                    return $ ptr `plusPtr` 1
                -- default attr is always visualized as the single character 'D'
                , default_attr_required_bytes = 1
                , serialize_default_attr = \ptr -> do
                    poke ptr (toEnum $ fromEnum 'D')
                    return $ ptr `plusPtr` 1
                -- row end is always visualized as the single character 'E'
                , row_end_required_bytes = 1
                , serialize_row_end = \ptr -> do
                    poke ptr (toEnum $ fromEnum 'E')
                    return $ ptr `plusPtr` 1
                , inline_hack = return ()
                }
            }
    return (out_ref, t)

