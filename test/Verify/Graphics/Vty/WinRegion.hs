{-# LANGUAGE ScopedTypeVariables #-}
module Verify.Graphics.Vty.WinRegion ( module Verify.Graphics.Vty.WinRegion
                                     , module Graphics.Vty.WinRegion
                                     )
    where

import Graphics.Vty.Debug
import Graphics.Vty.WinRegion

import Verify

import Data.Word

data EmptyWindow = EmptyWindow DebugWindow

instance Arbitrary EmptyWindow where
    arbitrary = return $ EmptyWindow (DebugWindow (0 :: Word) (0 :: Word))

instance Show EmptyWindow where
    show (EmptyWindow _) = "EmptyWindow"

instance Arbitrary DebugWindow where
    arbitrary = do
        w <- arbitrary
        h <- arbitrary
        return $ DebugWindow w h

