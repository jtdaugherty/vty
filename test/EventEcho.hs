module Main where

import Graphics.Vty

import Control.Applicative
import Control.Arrow
import Control.Monad.RWS

import Data.Sequence (Seq, (<|) )
import qualified Data.Sequence as Seq
import Data.Foldable

eventBufferSize = 1000

type App = RWST Vty () (Seq String) IO

main = do
    vty <- mkVty defaultConfig
    _ <- execRWST (vtyInteract False) vty Seq.empty
    shutdown vty

vtyInteract :: Bool -> App ()
vtyInteract shouldExit = do
    updateDisplay
    unless shouldExit $ handleNextEvent >>= vtyInteract

updateDisplay :: App ()
updateDisplay = do
    let info = string defAttr "Press ESC to exit."
    eventLog <- foldMap (string defAttr) <$> get
    let pic = picForImage $ info <-> eventLog
    vty <- ask
    liftIO $ update vty pic

handleNextEvent = ask >>= liftIO . nextEvent >>= handleEvent
    where
        handleEvent e               = do
            modify $ (<|) (show e) >>> Seq.take eventBufferSize
            return $ e == EvKey KEsc []

