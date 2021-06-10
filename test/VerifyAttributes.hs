module VerifyAttributes where

import Verify

import Graphics.Vty.Attributes(MaybeDefault(Default, KeepCurrent, SetTo))

instance Arbitrary a => Arbitrary (MaybeDefault a) where
  arbitrary = oneof [pure Default, pure KeepCurrent, SetTo <$> arbitrary]

oldSem :: MaybeDefault a -> MaybeDefault a -> MaybeDefault a
oldSem Default Default = Default
oldSem Default KeepCurrent = Default
oldSem Default (SetTo v) = SetTo v
oldSem KeepCurrent Default = Default
oldSem KeepCurrent KeepCurrent = KeepCurrent
oldSem KeepCurrent (SetTo v) = SetTo v
oldSem (SetTo _v) Default = Default
oldSem (SetTo v) KeepCurrent = SetTo v
oldSem (SetTo _) (SetTo v) = SetTo v

sameSemigroupValue :: MaybeDefault Int -> MaybeDefault Int -> Bool
sameSemigroupValue xa xb = xa <> xb == oldSem xa xb

tests :: IO [Test]
tests = return
  [ verify "check that the new Semigroup of MaybeDefault is equivalent to the old one" sameSemigroupValue
  ]

