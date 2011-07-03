module Main where

import Graphics.Vty hiding ( pad )

import Control.Concurrent( threadDelay )
import Control.Monad( liftM2 )

import Data.List
import Data.Word

import System.Environment( getArgs )
import System.IO
import System.Random


main = do 
    let fixed_gen = mkStdGen 0
    setStdGen fixed_gen
    args <- getArgs
    case args of
        []         -> mkVty >>= liftM2 (>>) (run $ Just 0) shutdown
        ["--delay"]         -> mkVty >>= liftM2 (>>) (run $ Just 1000000) shutdown
        ["--slow"] -> mkVty >>= liftM2 (>>) (run Nothing ) shutdown
        _          -> fail "usage: ./Bench [--slow|--delay]"

run (Just delay) vt  = mapM_ (\p -> update vt p >> threadDelay delay) . benchgen =<< display_bounds (terminal vt)
run Nothing vt = mapM_ (update vt) (benchgen $ DisplayRegion 200 100)

-- Currently, we just do scrolling.
takem :: (a -> Word) -> Word -> [a] -> ([a],[a])
takem len n [] = ([],[])
takem len n (x:xs) | lx > n = ([], x:xs)
                   | True = let (tk,dp) = takem len (n - lx) xs in (x:tk,dp)
    where lx = len x

fold :: (a -> Word) -> [Word] -> [a] -> [[a]]
fold len [] xs = []
fold len (ll:lls) xs = let (tk,dp) = takem len ll xs in tk : fold len lls dp

lengths :: Word -> StdGen -> [Word]
lengths ml g = 
    let (x,g2) = randomR (0,fromEnum ml) g 
        (y,g3) = randomR (0,x) g2 
    in (toEnum y) : lengths ml g3

nums :: StdGen -> [(Attr, String)]
nums g = let (x,g2) = (random g :: (Int, StdGen))
             (c,g3) = random g2
         in ( if c then def_attr `with_fore_color` red else def_attr
            , shows x " "
            ) : nums g3

pad :: Word -> Image -> Image
pad ml img = img <|> char_fill def_attr ' ' (ml - image_width img) 1

clines :: StdGen -> Word -> [Image]
clines g maxll = map (pad maxll . horiz_cat . map (uncurry string)) $ fold (toEnum . length . snd) (lengths maxll g1) (nums g2)
  where (g1,g2)  = split g

benchgen :: DisplayRegion -> [Picture]
benchgen (DisplayRegion w h) = take 500 $ map ((\i -> pic_for_image i) . vert_cat . take (fromEnum h)) $ tails $ clines (mkStdGen 42) w

