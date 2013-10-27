module Main where

import Graphics.Vty

import Control.Applicative
import Control.Arrow
import Control.Monad.RWS

import Data.Sequence (Seq, (<|) )
import qualified Data.Sequence as Seq
import Data.Foldable

event_buffer_size = 1000

type App = RWST Vty () (Seq String) IO

main = do
    vty <- mkVty
    _ <- execRWST vty_interact vty Seq.empty
    shutdown vty

vty_interact :: App ()
vty_interact = do
    update_display
    done <- handle_next_event
    unless done vty_interact

update_display :: App ()
update_display = do
    let info = string def_attr "Press ESC to exit."
    event_log <- foldMap (string def_attr) <$> get
    let pic = pic_for_layers [info,event_log]
    vty <- ask
    liftIO $ update vty pic

handle_next_event = ask >>= liftIO . next_event >>= handle_event
    where
        handle_event (EvKey KEsc []) = return True
        handle_event e               = do
            modify $ (<|) (show e) >>> Seq.take event_buffer_size
            return False

