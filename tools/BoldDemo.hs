module Main where

import Control.Monad (void)
import Graphics.Vty

main :: IO ()
main = do
    let img = horizCat [ string defAttr "not bold "
                       , string (defAttr `withStyle` bold) "bold"
                       , string defAttr " not bold"
                       ]
    cfg <- standardIOConfig
    vty <- mkVty cfg
    update vty $ picForImage img
    void $ nextEvent vty
    shutdown vty
