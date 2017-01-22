{-# OPTIONS_HADDOCK hide #-}
-- Copyright 2009 Corey O'Connor
module Data.Sequence.Extra where

import Data.Sequence

import Control.Parallel.Strategies

instance NFData a => NFData (Seq a) where
    rnf = \v -> rnf' (viewl v)
        where
            rnf' EmptyL = ()
            rnf' (a :< r) = rnf a >| rnf' (viewl r)
