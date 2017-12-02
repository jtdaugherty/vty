{-# LANGUAGE BangPatterns #-}

-- | The inline module provides a limited interface to changing the
-- style of terminal output. The intention is for this interface to be
-- used inline with other output systems.
--
-- The changes specified by the InlineM monad are applied to the
-- terminal's display attributes. These display attributes affect the
-- display of all following text output to the terminal file descriptor.
--
-- For example, in an IO monad the following code will print the text
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
-- 'putAttrChange' emits the control codes to the terminal device
-- attached to 'Handle'. This is a duplicate of the 'stdout' handle when
-- the 'terminalHandle' was (first) acquired. If 'stdout' has since been
-- changed then 'putStr', 'putStrLn', 'print' etc. will output to a
-- different 'Handle' than 'putAttrChange'.
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

type InlineM v = State InlineState v

data InlineState =
    InlineState { inlineAttr :: Attr
                , inlineUrlsEnabled :: Bool
                }

-- | Set the background color to the provided 'Color'.
backColor :: Color -> InlineM ()
backColor c = modify $ \s ->
    s { inlineAttr = inlineAttr s `mappend` (currentAttr `withBackColor` c)
      }

-- | Set the foreground color to the provided 'Color'.
foreColor :: Color -> InlineM ()
foreColor c = modify $ \s ->
    s { inlineAttr = inlineAttr s `mappend` (currentAttr `withForeColor` c)
      }

-- | Attempt to change the 'Style' of the following text..
--
-- If the terminal does not support the style change then no error is
-- produced. The style can still be removed.
applyStyle :: Style -> InlineM ()
applyStyle st = modify $ \s ->
    s { inlineAttr = inlineAttr s `mappend` (currentAttr `withStyle` st)
      }

-- | Attempt to remove the specified 'Style' from the display of the
-- following text.
--
-- This will fail if 'applyStyle' for the given style has not been
-- previously called.
removeStyle :: Style -> InlineM ()
removeStyle sMask = modify $ \s ->
    s { inlineAttr =
          let style' = case attrStyle (inlineAttr s) of
                Default -> error $ "Graphics.Vty.Inline: Cannot removeStyle if applyStyle never used."
                KeepCurrent -> error $ "Graphics.Vty.Inline: Cannot removeStyle if applyStyle never used."
                SetTo st -> st .&. complement sMask
          in (inlineAttr s) { attrStyle = SetTo style' }
      }

-- | Reset the display attributes.
defaultAll :: InlineM ()
defaultAll = modify $ \s -> s { inlineAttr = defAttr }

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
                    liftIO $ outputByteBuffer out $ writeToByteString $ writeDefaultAttr dc False
                    return $ FixedAttr defaultStyleMask Nothing Nothing Nothing
                Just v -> return v
    let InlineState attr urlsEnabled = execState c (InlineState currentAttr False)
        attr' = limitAttrForDisplay out attr
        fattr' = fixDisplayAttr fattr attr'
        diffs = displayAttrDiffs fattr fattr'
    outputByteBuffer out $ writeToByteString $ writeSetAttr dc urlsEnabled fattr attr' diffs
    modifyIORef (assumedStateRef out) $ \s -> s { prevFattr = Just fattr' }
    inlineHack dc

-- | Apply the provided display attributes changes to the terminal
-- output device.
--
-- This will flush the terminal output.
putAttrChange_ :: ( Applicative m, MonadIO m ) => InlineM () -> m ()
putAttrChange_ c = liftIO $ withOutput $ \out -> do
    hFlush stdout
    putAttrChange out c
    hFlush stdout
