{-# LANGUAGE TemplateHaskell #-}
module Main where

import Graphics.Vty

import Data.Array
import Data.Default (def)

import Control.Applicative
import Control.Monad
import Control.Monad.RWS

import System.Random

data Dude = Dude
    { dudeX :: Int
    , dudeY :: Int
    } deriving (Show,Eq)

data World = World
    { dude :: Dude
    , level :: Level
    }
    deriving (Show,Eq)

data Level = Level
    { levelStart :: (Int, Int)
    , levelEnd :: (Int, Int)
    , levelGeo :: Geo
    -- building the geo image is expensive. Cache it. Though VTY should go through greater lengths
    -- to avoid the need to cache images.
    , levelGeoImage :: Image
    }
    deriving (Show,Eq)

data LevelPiece
    = EmptySpace
    | Rock
    deriving (Show, Eq)

type Game = RWST Vty () World IO
type Geo = Array (Int, Int) LevelPiece

main :: IO ()
main = do
    vty <- mkVty def
    level0 <- mkLevel 1
    let world0 = World (Dude (fst $ levelStart level0) (snd $ levelStart level0)) level0
    (_finalWorld, ()) <- execRWST (play >> updateDisplay) vty world0
    shutdown vty

mkLevel :: Int -> IO Level
mkLevel difficulty = do
    let size = 80 * difficulty
    [levelWidth, levelHeight] <- replicateM 2 $ randomRIO (size,size)
    let randomP = (,) <$> randomRIO (2, levelWidth-3) <*> randomRIO (2, levelHeight-3)
    start <- randomP
    end <- randomP
    -- first the base geography: all rocks
    let baseGeo = array ((0,0), (levelWidth-1, levelHeight-1))
                        [((x,y),Rock) | x <- [0..levelWidth-1], y <- [0..levelHeight-1]]
    -- next the empty spaces that make the rooms
    -- for this we generate a number of center points
    centers <- replicateM (2 ^ difficulty + difficulty) randomP
    -- generate rooms for all those points, plus the start and end
    geo <- foldM (addRoom levelWidth levelHeight) baseGeo (start : end : centers)
    return $ Level start end geo (buildGeoImage geo)

addRoom :: Int -> Int -> Geo -> (Int, Int) -> IO Geo
addRoom levelWidth levelHeight geo (centerX, centerY) = do
    size <- randomRIO (5,15)
    let xMin = max 1 (centerX - size)
        xMax = min (levelWidth - 1) (centerX + size)
        yMin = max 1 (centerY - size)
        yMax = min (levelHeight - 1) (centerY + size)
    let room = [((x,y), EmptySpace) | x <- [xMin..xMax - 1], y <- [yMin..yMax - 1]]
    return (geo // room)

imageForGeo :: LevelPiece -> Image
imageForGeo EmptySpace = char (defAttr `withBackColor` green) ' '
imageForGeo Rock = char defAttr 'X'

pieceA, dumpA :: Attr
pieceA = defAttr `withForeColor` blue `withBackColor` green
dumpA = defAttr `withStyle` reverseVideo

play :: Game ()
play = do
    updateDisplay
    done <- processEvent
    unless done play

processEvent :: Game Bool
processEvent = do
    k <- ask >>= liftIO . nextEvent
    if k == EvKey KEsc []
        then return True
        else do
            case k of
                EvKey (KChar 'r') [MCtrl]  -> ask >>= liftIO . refresh
                EvKey KLeft  []            -> moveDude (-1) 0
                EvKey KRight []            -> moveDude 1 0
                EvKey KUp    []            -> moveDude 0 (-1)
                EvKey KDown  []            -> moveDude 0 1
                _                          -> return ()
            return False

moveDude :: Int -> Int -> Game ()
moveDude dx dy = do
    world <- get
    let Dude x y = dude world
    let x' = x + dx
        y' = y + dy
    -- this is only valid because the level generation assures the border is always Rock
    case levelGeo (level world) ! (x',y') of
        EmptySpace -> put $ world { dude = Dude x' y' }
        _          -> return ()

updateDisplay :: Game ()
updateDisplay = do
    let info = string defAttr "Move with the arrows keys. Press ESC to exit."
    -- determine offsets to place the dude in the center of the level.
    (w,h) <- asks outputIface >>= liftIO . displayBounds
    theDude <- gets dude
    let ox = (w `div` 2) - dudeX theDude
        oy = (h `div` 2) - dudeY theDude
    -- translate the world images to place the dude in the center of the level.
    world' <- map (translate ox oy) <$> worldImages
    let pic = picForLayers $ info : world'
    vty <- ask
    liftIO $ update vty pic

worldImages :: Game [Image]
worldImages = do
    theDude <- gets dude
    theLevel <- gets level
    let dudeImage = translate (dudeX theDude) (dudeY theDude) (char pieceA '@')
    return [dudeImage, levelGeoImage theLevel]

buildGeoImage :: Geo -> Image
buildGeoImage geo =
    let (geoWidth, geoHeight) = snd $ bounds geo
    -- seems like a the repeated index operation should be removable. This is not performing random
    -- access but (presumably) access in order of index.
    in vertCat [ geoRow
               | y <- [0..geoHeight-1]
               , let geoRow = horizCat [ i
                                       | x <- [0..geoWidth-1]
                                       , let i = imageForGeo (geo ! (x,y))
                                       ]
               ]
