{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
module VerifyWin32 where

import Prelude hiding ( catch )

import Verify.Graphics.Vty.Output
import Verify

tests :: IO [Test]
#ifndef WINDOWS
tests = return mempty
#else
tests = return []
#endif
