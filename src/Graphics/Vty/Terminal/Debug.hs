-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Vty.Terminal.Debug ( DebugTerminal(..)
                                   , DebugDisplay(..)
                                   , terminal_instance
                                   , dehandle
                                   )
    where

import Graphics.Vty.DisplayRegion
import Graphics.Vty.Terminal.Generic

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State.Strict

import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BSCore
import Data.IORef
import qualified Data.Sequence as Seq
import qualified Data.String.UTF8 as UTF8
import Data.Word

import Foreign.Marshal.Array ( peekArray )
import Foreign.Ptr ( plusPtr )
import Foreign.Storable ( poke )

import Unsafe.Coerce

-- | The debug display terminal produces a string representation of the requested picture.  There is
-- *not* an isomorphism between the string representation and the picture.  The string
-- representation is a simplification of the picture that is only useful in debugging VTY without
-- considering terminal specific issues.
--
-- The debug implementation is useful in manually determining if the sequence of terminal operations
-- matches the expected sequence. So requirement of the produced representation is simplicity in
-- parsing the text representation and determining how the picture was mapped to terminal
-- operations.
--
-- All terminals support the operations specified in the Terminal class defined in
-- Graphics.Vty.Terminal. As an instance of the Terminal class is also an instance of the Monad
-- class there exists a monoid that defines it's algebra. The string representation is a sequence of
-- identifiers where each identifier is the name of an operation in the algebra.

data DebugTerminal = DebugTerminal
    { debug_terminal_last_output :: IORef (UTF8.UTF8 BS.ByteString)
    , debug_terminal_bounds :: DisplayRegion
    } 

instance Terminal DebugTerminal where
    terminal_ID _t = "debug_terminal"
    release_terminal _t = return ()
    reserve_display _t = return ()
    release_display _t = return ()
    display_bounds t = return $ debug_terminal_bounds t
    display_terminal_instance t r c = return $ c (DebugDisplay r)
    output_byte_buffer t out_buffer buffer_size 
        =   liftIO $ do
            putStrLn $ "output_byte_buffer ?? " ++ show buffer_size
            peekArray (fromEnum buffer_size) out_buffer 
            >>= return . UTF8.fromRep . BSCore.pack
            >>= writeIORef (debug_terminal_last_output t)

data DebugDisplay = DebugDisplay
    { debug_display_bounds :: DisplayRegion
    } 

terminal_instance :: ( Applicative m, MonadIO m ) => DisplayRegion -> m TerminalHandle
terminal_instance r = do
    output_ref <- liftIO $ newIORef undefined
    new_terminal_handle $ DebugTerminal output_ref r

dehandle :: TerminalHandle -> DebugTerminal
dehandle (TerminalHandle t _) = unsafeCoerce t

instance DisplayTerminal DebugDisplay where
    -- | Provide the current bounds of the output terminal.
    context_region d = debug_display_bounds d

    -- | A cursor move is always visualized as the single character 'M'
    move_cursor_required_bytes _d _x _y = 1

    -- | A cursor move is always visualized as the single character 'M'
    serialize_move_cursor _d _x _y ptr = do
        liftIO $ poke ptr (toEnum $ fromEnum 'M') 
        return $ ptr `plusPtr` 1

    -- | Show cursor is always visualized as the single character 'S'
    show_cursor_required_bytes _d = 1

    -- | Show cursor is always visualized as the single character 'S'
    serialize_show_cursor _d ptr = do
        liftIO $ poke ptr (toEnum $ fromEnum 'S') 
        return $ ptr `plusPtr` 1

    -- | Hide cursor is always visualized as the single character 'H'
    hide_cursor_required_bytes _d = 1

    -- | Hide cursor is always visualized as the single character 'H'
    serialize_hide_cursor _d ptr = do
        liftIO $ poke ptr (toEnum $ fromEnum 'H') 
        return $ ptr `plusPtr` 1

    -- | An attr change is always visualized as the single character 'A'
    attr_required_bytes _d _fattr _diffs _attr = 1

    -- | An attr change is always visualized as the single character 'A'
    serialize_set_attr _d _fattr _diffs _attr ptr = do
        liftIO $ poke ptr (toEnum $ fromEnum 'A')
        return $ ptr `plusPtr` 1

    default_attr_required_bytes _d = 1
    serialize_default_attr _d ptr = do
        liftIO $ poke ptr (toEnum $ fromEnum 'D')
        return $ ptr `plusPtr` 1
        
