module Verify.Graphics.Vty.Prelude
  ( module Verify.Graphics.Vty.Prelude
  , MockWindow(..)
  )
where

import Graphics.Vty.Debug

import Verify

data EmptyWindow = EmptyWindow MockWindow

instance Arbitrary EmptyWindow where
    arbitrary = return $ EmptyWindow (MockWindow (0 :: Int) (0 :: Int))

instance Show EmptyWindow where
    show (EmptyWindow _) = "EmptyWindow"

instance Arbitrary MockWindow where
    arbitrary = do
        w <- choose (1,1024)
        h <- choose (1,1024)
        return $ MockWindow w h

