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

data Dude = Dude
    { dude_x :: Int
    , dude_y :: Int
    } deriving (Show,Eq)

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
    (_final_world, ()) <- execRWST (play >> view) vty world_0
    shutdown vty

mkLevel _difficulty = do
    level_width <- randomRIO (40,80)
    level_height <- randomRIO (40,80)
    start <- (,) <$> randomRIO (2, level_width-3) <*> randomRIO (2, level_height-3)
    end <- (,) <$> randomRIO (2, level_width-3) <*> randomRIO (2, level_height-3)
    -- first the base geography: all rocks
    let base_geo = array ((0,0), (level_width, level_height))
                         [((x,y),Rock) | x <- [0..level_width-1], y <- [0..level_height-1]]
    -- next the empty spaces that make the rooms
    geo <- add_room start base_geo level_width level_height
    return $ Level start end geo

add_room (center_x, center_y) geo level_width level_height = do
    size <- randomRIO (5,15)
    let x_min = max 1 (center_x - size)
        x_max = min (level_width - 1) (center_x + size)
        y_min = max 1 (center_y - size)
        y_max = min (level_height - 1) (center_y + size)
    let room = [((x,y), EmptySpace) | x <- [x_min..x_max - 1], y <- [y_min..y_max - 1]]
    return (geo // room)

image_for_geo EmptySpace = char (def_attr `with_back_color` green) ' '
image_for_geo Rock = char (def_attr `with_fore_color` white) 'X'

pieceA = def_attr `with_fore_color` red
dumpA = def_attr `with_style` reverse_video

play = do
    view
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
    let x' = x + dx
        y' = y + dy
    -- this is only valid because the level generation assures the border is always Rock
    case geo (level world) ! (x',y') of
        EmptySpace -> put $ world { dude = Dude x' y' }
        _          -> return ()

view :: Game ()
view = do
    let info = string def_attr "Move with the arrows keys. Press ESC to exit."
    -- determine offsets to place the dude in the center of the level.
    DisplayRegion w h <- asks terminal >>= liftIO . display_bounds
    the_dude <- gets dude
    let ox = (w `div` 2) - dude_x the_dude
        oy = (h `div` 2) - dude_y the_dude + image_height info
    -- translate the world images to place the dude in the center of the level.
    world' <- map (translate ox oy) <$> world
    let pic = pic_for_layers $ info : world'
    vty <- ask
    liftIO $ update vty pic

world :: Game [Image]
world = do
    the_dude <- gets dude
    the_level <- gets level
    let dude_image = translate (dude_x the_dude) (dude_y the_dude) (char pieceA '@')
        (geo_width, geo_height) = snd $ bounds (geo the_level)
        geo_image = vert_cat [ geo_row | y <- [0..geo_height-1],
                               let geo_row = horiz_cat [ i | x <- [0..geo_width-1],
                                                         let i = image_for_geo (geo the_level ! (x,y))
                                                       ]
                             ]
    return [dude_image, geo_image]
