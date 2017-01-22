{-# LANGUAGE BangPatterns #-}

-- | The inline module provides a limited interface to changing the
-- style of terminal output. The intention is for this interface to be
-- used inline with other output systems.
--
-- The changes specified by the InlineM monad are applied to the
-- terminals display attributes. These display attributes effect the
-- display of all following text output to the terminal file descriptor.
--
-- For example, in an IO monad the following code with print the text
-- \"Not styled. \" Followed by the text \" Styled! \" drawn over a red
-- background and underlined.
--
-- @
--      putStr \"Not styled. \"
--      putAttrChange_ $ do
--          backColor red
--          applyStyle underline
--      putStr \" Styled! \"
--      putAttrChange_ $ defaultAll
--      putStrLn \"Not styled.\"
-- @
--
-- 'putAttrChange' outputs the control codes to the terminal device
-- 'Handle'. This is a duplicate of the 'stdout' handle when the
-- 'terminalHandle' was (first) acquired. If 'stdout' has since been
-- changed then 'putStr', 'putStrLn', 'print' etc.. will output to a
-- different 'Handle' than 'putAttrChange'
--
-- Copyright 2009-2010 Corey O'Connor
module Graphics.Vty.Inline
  ( module Graphics.Vty.Inline
  , withVty
  )
where

import Graphics.Vty
import Graphics.Vty.DisplayAttributes
import Graphics.Vty.Inline.Unsafe

import Blaze.ByteString.Builder (writeToByteString)

import Control.Monad.State.Strict

import Data.Bits ( (.&.), complement )
import Data.IORef

import System.IO

type InlineM v = State Attr v

-- | Set the background color to the provided 'Color'
backColor :: Color -> InlineM ()
backColor c = modify $ flip mappend ( currentAttr `withBackColor` c )

-- | Set the foreground color to the provided 'Color'
foreColor :: Color -> InlineM ()
foreColor c = modify $ flip mappend ( currentAttr `withForeColor` c )

-- | Attempt to change the 'Style' of the following text.
--
-- If the terminal does not support the style change no error is
-- produced. The style can still be removed.
applyStyle :: Style -> InlineM ()
applyStyle s = modify $ flip mappend ( currentAttr `withStyle` s )

-- | Attempt to remove the specified 'Style' from the display of the
-- following text.
--
-- This will fail if applyStyle for the given style has not been
-- previously called.
removeStyle :: Style -> InlineM ()
removeStyle sMask = modify $ \attr ->
    let style' = case attrStyle attr of
                    Default -> error $ "Graphics.Vty.Inline: Cannot removeStyle if applyStyle never used."
                    KeepCurrent -> error $ "Graphics.Vty.Inline: Cannot removeStyle if applyStyle never used."
                    SetTo s -> s .&. complement sMask
    in attr { attrStyle = SetTo style' }

-- | Reset the display attributes
defaultAll :: InlineM ()
defaultAll = put defAttr

-- | Apply the provided display attribute changes to the given terminal
-- output device.
--
-- This does not flush the terminal.
putAttrChange :: ( Applicative m, MonadIO m ) => Output -> InlineM () -> m ()
putAttrChange out c = liftIO $ do
    bounds <- displayBounds out
    dc <- displayContext out bounds
    mfattr <- prevFattr <$> readIORef (assumedStateRef out)
    fattr <- case mfattr of
                Nothing -> do
                    liftIO $ outputByteBuffer out $ writeToByteString $ writeDefaultAttr dc
                    return $ FixedAttr defaultStyleMask Nothing Nothing
                Just v -> return v
    let attr = execState c currentAttr
        attr' = limitAttrForDisplay out attr
        fattr' = fixDisplayAttr fattr attr'
        diffs = displayAttrDiffs fattr fattr'
    outputByteBuffer out $ writeToByteString $ writeSetAttr dc fattr attr' diffs
    modifyIORef (assumedStateRef out) $ \s -> s { prevFattr = Just fattr' }
    inlineHack dc

-- | Apply the provided display attributes changes to the terminal
-- output device that was current at the time this was first used.
-- Which, for most use cases, is the current terminal.
--
-- This will flush the terminal output.
putAttrChange_ :: ( Applicative m, MonadIO m ) => InlineM () -> m ()
putAttrChange_ c = liftIO $ withOutput $ \out -> do
    hFlush stdout
    putAttrChange out c
    hFlush stdout
