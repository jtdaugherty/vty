module Main where

import Graphics.Vty

import Data.Array
import qualified Data.ByteString as B
import Data.Word

import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Writer

import System.IO
import System.Random

data Dude = Dude Int Int
    deriving (Show,Eq)

data World = World
    { dude :: Dude
    , level :: Level
    }
    deriving (Show,Eq)

data Level = Level
    { start :: (Int, Int)
    , end :: (Int, Int)
    , geo :: Array (Int, Int) LevelPiece
    }
    deriving (Show,Eq)

data LevelPiece
    = EmptySpace
    | Rock
    deriving (Show, Eq)

type Game = RWST Vty () World IO

main = do 
    vty <- mkVty
    level_0 <- mkLevel 0
    let world_0 = World (Dude (fst $ start level_0) (snd $ start level_0)) level_0
    (_final_world, ()) <- execRWST (play >> view_world) vty world_0
    shutdown vty

mkLevel _difficulty = do
    level_width <- randomRIO (10,15)
    level_height <- randomRIO (10,15)
    start <- (,) <$> randomRIO (2, level_width-3) <*> randomRIO (2, level_height-3)
    end <- (,) <$> randomRIO (2, level_width-3) <*> randomRIO (2, level_height-3)
    let geo = array ((0,0), (level_width, level_height))
                    [((x,y),p) | x <- [0..level_width-1], y <- [0..level_height-1],
                                 let p = if (x == 0 || x == level_width-1) || (y==0 || y==level_height-1)
                                            then Rock
                                            else EmptySpace
                    ]
    return $ Level start end geo

image_for_geo EmptySpace = char (def_attr `with_back_color` green) ' '
image_for_geo Rock = char (def_attr `with_fore_color` white) 'X'

pieceA = def_attr `with_fore_color` red
dumpA = def_attr `with_style` reverse_video

play = do
    view_world
    done <- process_event
    unless done play

process_event = do
    k <- ask >>= liftIO . next_event
    if k == EvKey KEsc []
        then return True
        else do
            case k of
                EvKey (KASCII 'r') [MCtrl] -> ask >>= liftIO . refresh
                EvKey KLeft  []            -> move_dude (-1) 0
                EvKey KRight []            -> move_dude 1 0
                EvKey KUp    []            -> move_dude 0 (-1)
                EvKey KDown  []            -> move_dude 0 1
                _                          -> return ()
            return False

move_dude dx dy = do
    vty <- ask
    world <- get
    let Dude x y = dude world
    (w, h) <- gets (snd . bounds . geo . level)
    put $ world { dude = Dude (min (w - 2) $ max 1 (x + dx))
                              (min (h - 2) $ max 1 (y + dy))
                }

view_world :: Game ()
view_world = do
    Dude x y <- gets dude
    the_level <- gets level
    let dude_image = translate x y (char pieceA '@')
    let (geo_width, geo_height) = snd $ bounds (geo the_level)
        geo_image = vert_cat [ geo_row | y <- [0..geo_height-1],
                               let geo_row = horiz_cat [ i | x <- [0..geo_width-1],
                                                         let i = image_for_geo (geo the_level ! (x,y))
                                                       ]
                             ]
        info = string def_attr "Move with the arrows keys. Press ESC to exit."
    let pic = pic_for_layers [info, translate 0 1 dude_image,translate 0 1 geo_image]
    vty <- ask
    liftIO $ update vty pic
