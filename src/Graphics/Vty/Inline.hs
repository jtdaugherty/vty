-- | The inline module provides a limited interface to changing the style of terminal output. The
-- intention is for this interface to be used inline with other output systems. 
--
-- The changes specified by the InlineM monad are applied to the terminals display attributes. These
-- display attributes effect the display of all following text output to the terminal file
-- descriptor.
--
-- For example, in an IO monad the following code with print the text \"Not styled. \" Followed by the
-- text \" Styled! \" drawn over a red background and underlined.
--
-- @
--      putStr \"Not styled. \"
--      put_attr_change_ $ do
--          back_color red 
--          apply_style underline
--      putStr \" Styled! \"
--      put_attr_change_ $ default_all
--      putStrLn \"Not styled.\"
-- @
--
-- 'put_attr_change' outputs the control codes to the terminal device 'Handle'. This is a duplicate
-- of the 'stdout' handle when the 'terminal_handle' was (first) acquired. If 'stdout' has since been
-- changed then 'putStr', 'putStrLn', 'print' etc.. will output to a different 'Handle' than
-- 'put_attr_change'
--
-- Copyright 2009-2010 Corey O'Connor
{-# LANGUAGE BangPatterns #-}
module Graphics.Vty.Inline ( module Graphics.Vty.Inline
                           , withVty
                           )
    where

import Graphics.Vty
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Inline.Unsafe
import Graphics.Vty.Output.Interface

import Blaze.ByteString.Builder (writeToByteString)

import Control.Applicative
import Control.Monad.State.Strict

import Data.Bits ( (.&.), complement )
import Data.IORef
import Data.Monoid ( mappend )

import System.IO

type InlineM v = State Attr v

-- | Set the background color to the provided 'Color'
back_color :: Color -> InlineM ()
back_color c = modify $ flip mappend ( current_attr `with_back_color` c )

-- | Set the foreground color to the provided 'Color'
fore_color :: Color -> InlineM ()
fore_color c = modify $ flip mappend ( current_attr `with_fore_color` c )

-- | Attempt to change the 'Style' of the following text.
--
-- If the terminal does not support the style change no error is produced. The style can still be
-- removed.
apply_style :: Style -> InlineM ()
apply_style s = modify $ flip mappend ( current_attr `with_style` s )

-- | Attempt to remove the specified 'Style' from the display of the following text.
--
-- This will fail if apply_style for the given style has not been previously called. 
remove_style :: Style -> InlineM ()
remove_style s_mask = modify $ \attr -> 
    let style' = case attr_style attr of
                    Default -> error $ "Graphics.Vty.Inline: Cannot remove_style if apply_style never used."
                    KeepCurrent -> error $ "Graphics.Vty.Inline: Cannot remove_style if apply_style never used."
                    SetTo s -> s .&. complement s_mask
    in attr { attr_style = SetTo style' } 

-- | Reset the display attributes
default_all :: InlineM ()
default_all = put def_attr

-- | Apply the provided display attribute changes to the given terminal.
--
-- This does not flush the terminal.
put_attr_change :: ( Applicative m, MonadIO m ) => Output -> InlineM () -> m ()
put_attr_change out c = liftIO $ do
    bounds <- display_bounds out
    dc <- display_context out bounds
    mfattr <- prev_fattr <$> readIORef (assumed_state_ref out)
    fattr <- case mfattr of
                Nothing -> do
                    liftIO $ output_byte_buffer out $ writeToByteString $ write_default_attr dc
                    return $ FixedAttr default_style_mask Nothing Nothing
                Just v -> return v
    let attr = execState c current_attr
        attr' = limit_attr_for_display out attr
        fattr' = fix_display_attr fattr attr'
        diffs = display_attr_diffs fattr fattr'
    output_byte_buffer out $ writeToByteString $ write_set_attr dc fattr attr' diffs
    modifyIORef (assumed_state_ref out) $ \s -> s { prev_fattr = Just fattr' }
    inline_hack dc

-- | Apply the provided display attributes changes to the terminal that was current at the time this
-- was first used. Which, for most use cases, is the current terminal.
--
-- This will flush the terminal output.
put_attr_change_ :: ( Applicative m, MonadIO m ) => InlineM () -> m ()
put_attr_change_ c = liftIO $ do
    out <- withVty $ return . output_iface
    hFlush stdout
    put_attr_change out c
    hFlush stdout
