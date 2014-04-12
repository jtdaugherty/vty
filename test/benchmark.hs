{-# LANGUAGE QuasiQuotes #-}
module Main where

import Graphics.Vty

import qualified BenchImageFuzz
import qualified BenchNoDiffOpt
import qualified BenchRenderChar
import qualified BenchVerticalScroll

import Control.Monad

import Data.Maybe
import Data.List

import System.Environment
import System.Posix.Process

import Verify

main = do
    args <- getArgs
    let benches = [ ("no-diff-opt-0", BenchNoDiffOpt.bench0)
                  , ("render-char-0", BenchRenderChar.bench0)
                  , ("render-char-1", BenchRenderChar.bench1)
                  , ("vertical-scroll-0", BenchVerticalScroll.bench0)
                  , ("image-fuzz-0", BenchImageFuzz.bench0) ]
        help = forM_ benches $ \(b,_) -> putStrLn $ "--" ++ b
    case args of
        ["--help"] -> help
        ["-h"    ] -> help
        _          -> do
            let args' = if args /= []
                        then map (drop 2) args
                        else map fst benches
            -- drop the dash-dash "--"
            results <- forM args' $ \bName -> do
                case lookup bName benches of
                    Just b  -> bench bName b
                    Nothing -> fail $ "No benchmark named " ++ bName
            print results
            return ()

bench bName b = do
    putStrLn $ "starting " ++ bName
    Bench bDataGen bProc <- b
    bData <- bDataGen
    startTimes <- bData `deepseq` getProcessTimes
    bProc bData
    endTimes <- getProcessTimes
    let ut = userTime endTimes - userTime startTimes
        st = systemTime endTimes - systemTime startTimes
    putStrLn $ "user time: " ++ show ut
    putStrLn $ "system time: " ++ show st
    return (bName, ut, st)

