{-# LANGUAGE ScopedTypeVariables #-}
module Verify.Graphics.Vty.DisplayRegion ( module Verify.Graphics.Vty.DisplayRegion
                                         , module Graphics.Vty.DisplayRegion
                                         , DebugWindow(..)
                                         )
    where

import Graphics.Vty.Debug
import Graphics.Vty.DisplayRegion

import Verify

data EmptyWindow = EmptyWindow DebugWindow

instance Arbitrary EmptyWindow where
    arbitrary = return $ EmptyWindow (DebugWindow (0 :: Int) (0 :: Int))

instance Show EmptyWindow where
    show (EmptyWindow _) = "EmptyWindow"

instance Arbitrary DebugWindow where
    arbitrary = do
        w <- choose (1,1024)
        h <- choose (1,1024)
        return $ DebugWindow w h

