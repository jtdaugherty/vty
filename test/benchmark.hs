{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Graphics.Vty

import qualified BenchNoDiffOpt
import qualified BenchRenderChar
import qualified BenchVerticalScroll

import Control.Monad

import Data.Maybe
import Data.List

import System.Environment
import System.Posix.Process

main = do
    args <- getArgs
    let benches = [ ("no-diff-opt-0", BenchNoDiffOpt.bench_0)
                  , ("render-char-0", BenchRenderChar.bench_0)
                  , ("render-char-1", BenchRenderChar.bench_1)
                  , ("vertical-scroll-0", BenchVerticalScroll.bench_0)]
        help = forM_ benches $ \(b,_) -> putStrLn $ "--" ++ b
    case args of
        ["--help"] -> help
        ["-h"    ] -> help
        _          -> do
            let args' = if args /= []
                        then args
                        else map fst benches
            -- drop the dash-dash "--"
            results <- forM args' $ \(_ : _ : b) -> do
                case lookup b benches of
                    Just f  -> bench b f
                    Nothing -> fail $ "No benchmark named " ++ b
            print results
            return ()

bench b f = do
    putStrLn $ "starting " ++ b
    start_times <- getProcessTimes
    f
    end_times <- getProcessTimes
    let user_time = userTime end_times - userTime start_times
        system_time = systemTime end_times - systemTime start_times
    putStrLn $ "user time: " ++ show user_time
    putStrLn $ "system time: " ++ show system_time
    return (b, user_time, system_time)

