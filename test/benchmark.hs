{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    let benches = [ ("no-diff-opt-0", BenchNoDiffOpt.bench_0)
                  , ("render-char-0", BenchRenderChar.bench_0)
                  , ("render-char-1", BenchRenderChar.bench_1)
                  , ("vertical-scroll-0", BenchVerticalScroll.bench_0)
                  , ("image-fuzz-0", BenchImageFuzz.bench_0) ]
        help = forM_ benches $ \(b,_) -> putStrLn $ "--" ++ b
    case args of
        ["--help"] -> help
        ["-h"    ] -> help
        _          -> do
            let args' = if args /= []
                        then map (drop 2) args
                        else map fst benches
            -- drop the dash-dash "--"
            results <- forM args' $ \b_name -> do
                case lookup b_name benches of
                    Just b  -> bench b_name b
                    Nothing -> fail $ "No benchmark named " ++ b_name
            print results
            return ()

bench b_name b = do
    putStrLn $ "starting " ++ b_name
    Bench b_data_gen b_proc <- b
    b_data <- b_data_gen
    start_times <- b_data `deepseq` getProcessTimes
    b_proc b_data
    end_times <- getProcessTimes
    let user_time = userTime end_times - userTime start_times
        system_time = systemTime end_times - systemTime start_times
    putStrLn $ "user time: " ++ show user_time
    putStrLn $ "system time: " ++ show system_time
    return (b_name, user_time, system_time)

