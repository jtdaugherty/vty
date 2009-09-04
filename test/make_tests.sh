#!/bin/bash
set -e
GHC_ARGS='--make -i../src -ignore-package vty ../cbits/gwinsz.c ../cbits/set_term_timing.c  -funbox-strict-fields -Wall -threaded -fno-full-laziness -fspec-constr'

rm -f Bench.o Bench.hi Bench
ghc $GHC_ARGS '-prof' '-auto-all' Bench.hs

rm -f Bench2.o Bench2.hi Bench2
ghc $GHC_ARGS '-prof' '-auto-all' Bench2.hs

rm -f BenchRenderChar.o BenchRenderChar.hi BenchRenderChar
ghc $GHC_ARGS '-prof' '-auto-all' BenchRenderChar

rm -f Test.o Test.hi Test
ghc $GHC_ARGS Test

rm -f Test2.o Test2.hi Test2
ghc $GHC_ARGS Test2

rm -f BenchmarkImageOps.hi BenchmarkImageOps.o BenchmarkImageOps
ghc $GHC_ARGS BenchmarkImageOps

rm -f ControlTable.hi ControlTable.o ControlTable
ghc $GHC_ARGS ControlTable

rm -f yi_issue_264.hi yi_issue_264.o yi_issue_264
ghc $GHC_ARGS yi_issue_264

rm -f vty_issue_18.hi vty_issue_18.o vty_issue_18
ghc $GHC_ARGS vty_issue_18

