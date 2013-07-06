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
--      t <- terminal_handle
--      putStr \"Not styled. \"
--      put_attr_change t $ do
--          back_color red 
--          apply_style underline
--      putStr \" Styled! \"
--      put_attr_change t $ default_all
--      putStrLn \"Not styled.\"
--      release_terminal t
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
                           )
    where

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Terminal.Interface

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

-- | Apply the provided display attribute changes to the terminal.
--
-- This also flushes the 'stdout' handle.
put_attr_change :: ( Applicative m, MonadIO m ) => TerminalHandle -> InlineM () -> m ()
put_attr_change t c = do
    bounds <- display_bounds t
    d <- display_context t bounds
    mfattr <- liftIO $ known_fattr <$> readIORef ( state_ref t )
    fattr <- case mfattr of
                Nothing -> do
                    liftIO $ marshall_to_terminal t (default_attr_required_bytes d) (serialize_default_attr d) 
                    return $ FixedAttr default_style_mask Nothing Nothing
                Just v -> return v
    let attr = execState c current_attr
        attr' = limit_attr_for_display d attr
        fattr' = fix_display_attr fattr attr'
        diffs = display_attr_diffs fattr fattr'
    liftIO $ hFlush stdout
    liftIO $ marshall_to_terminal t ( attr_required_bytes d fattr attr' diffs )
                                    ( serialize_set_attr d fattr attr' diffs )
    liftIO $ modifyIORef ( state_ref t ) $ \s -> s { known_fattr = Just fattr' }
    inline_hack d
    liftIO $ hFlush stdout

