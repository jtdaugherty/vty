module Main where

import Control.Monad (void)
import Graphics.Vty

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let line1 = charFill (defAttr `withBackColor` blue) ' ' 10 1
      line2 = charFill (defAttr `withBackColor` green) ' ' 10 1
      img = translate 10 5 (line1 `vertJoin` line2)
      pic = picForImage img

  update vty pic
  void $ nextEvent vty
  shutdown vty
