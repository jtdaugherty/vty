module Main where

import Graphics.Vty
import Graphics.Text.Width (wcswidth)

import Control.Monad.RWS

import Data.Sequence (Seq)
import Data.List (unfoldr)
import qualified Data.Sequence as Seq


type App = RWST Vty () (Seq String) IO

main = do
    vty <- mkVty defaultConfig
    _ <- execRWST (vtyInteract False) vty Seq.empty
    shutdown vty

vtyInteract :: Bool -> App ()
vtyInteract shouldExit = do
    updateDisplay
    unless shouldExit $ handleNextEvent >>= vtyInteract

emoji = [
      "\x263A" -- Smiling Face
    , "\x2710" -- Upper Right Pencil
    , "\x2A" -- Asterisk
    , "\x261A" -- Black Left Pointing Index
    , "\x1F59C" -- Black Left Pointing Backhand Index
    , "\x1F59A" -- Sideways Black Left Pointing Index
    , "\x1F448" -- Backhand Index Pointing Left
    , "\x261C" -- White Left Pointing Index
    , "\x1F598" -- Sideways White Left Pointing Index
    , "\x261A\x1F3FB" -- Black Left Pointing Index + Emoji Modifier Fitzpatrick Type-1-2
    , "\x2657" -- White Chess Bishop
    , "\x270F" -- Pencil
    , "\x27A1" -- Right Arrow
    , "\x2611" -- Ballot Box With Check
    , "\x2714" -- Heavy Check Mark
    , "\x2049" -- Exclamation Question Mark
    , "\x2753" -- Question Mark
    , "\x274C" -- Cross Mark
    , "\x203C" -- Double Exclamation Mark
    , "\x2139" -- Information
    , "\x267B" -- Recycling Symbol
    , "\x2672" -- Universal Recycling Symbol
    , "\x2934" -- Right Arrow Curving Up
    , "\x2684" -- Die Face-5
    , "\x2B06" -- Up Arrow
    , "\x2691" -- Black Flag
    , "\x1F3F4" -- Waving Black Flag
    , "\x26A0" -- Warning
    , "\x2699" -- Gear
    , "\x2642" -- Male Sign
    , "\x26A3" -- Doubled Male Sign
    , "\x26B2" -- Neuter
    , "\x2B50" -- Star
    , "\x1F004" -- Mahjong Red Dragon
    , "\x2B1C" -- White Large Square
    , "\x1F01B" -- Mahjong Tile Three of Circles
    , "\x26B7" -- Chiron
    , "\x26F7" -- Skier
    , "\x26F7\x1F3FB" -- Skier, Type-1-2
    , "\x26BD" -- Soccer Ball
    , "\x26D4" -- No Entry
    , "\x2757" -- Exclamation Mark
    , "\x26DF" -- Black Truck
    , "\x26EB" -- Castle
    , "\x1F603" -- Grinning Face With Big Eyes
    , "\x1F600" -- Grinning Face
    , "\x1F55E" -- Three-Thirty
    , "\x1F4B0" -- Money Bag
    , "\x2795" -- Heavy Plus Sign
    , "\x2755" -- White Exclamation Mark
    , "\x2753" -- Question Mark
    , "\x2754" -- White Question Mark
    , "\x1F239" -- Japanese “Discount” Button
    , "\x1F532" -- Black Square Button
    , "\x1F1E6" -- Regional Indicator Symbol Letter A
    , "\x26E2" -- Astronomical Symbol for Uranus
    , "\x1F542" -- Cross Pommee
    , "\x1F596" -- Vulcan Salute
    , "\x1F596\x1F3FB" -- Vulcan Salute: Light Skin Tone
    , "\x1F324" -- Sun Behind Small Cloud
    , "\x1F5F8" -- Light Check Mark
    , "\x1F5D9" -- Cancellation X
    , "\x1F5F5" -- Ballot Box with Script X
    , "\x1F5F7" -- Ballot Box with Bold Script X
    , "\x1F5F9" -- Ballot Box with Bold Check
    , "\x1F5CF" -- Page
    , "\x1F5CE" -- Document
    , "\x1F5B9" -- Document with Text
    , "\x1F5CB" -- Empty Document
    , "\x1F5AE" -- Wired Keyboard
    , "\x1F5A6" -- Keyboard and Mouse
    , "\x1F591" -- Reversed Raised Hand with Fingers Splayed
    , "\x1F590" -- Hand With Fingers Splayed
    , "\x1F984" -- Unicorn Face
    , "\x1F54E" -- Menorah
    , "\x1F923" -- Rolling on the Floor Laughing
    , "\x2764" -- Red Heart
    , "\x1F499" -- Blue Heart
    , "\x1F9E1" -- Orange Heart
    , "\x1F5A4" -- Black Heart
    , "\x1F90D" -- White Heart
    , "\x2665" -- Heart Suit
    , "\x2763" -- Heavy Heart Exclamation
    , "\x1F494" -- Broken Heart
    , "\x1F929" -- Star-Struck
    , "\x1F9D2" -- Child
    , "\x1F9D2\x1F3FB" -- Child: Light Skin Tone
    , "\x1F466" -- Boy
    , "\x1F466\x1F3FB" -- Boy: Light Skin Tone
    , "\x1F467" -- Girl
    , "\x1F467\x1F3FB" -- Girl: Light Skin Tone
    , "\x20BF" -- Bitcoin Sign
    , "\x1F970" -- Smiling Face With Hearts
    , "\x1F468" -- Man
    , "\x1F468\x200D\x1F9B0" -- Man: Red Hair
    , "\x1F468\x1F3FB\x200D\x1F9B0" -- Man: Light Skin Tone, Red Hair
    , "\x1F468\x200D\x1F468\x200D\x1F467" -- Family: Man, Man, Girl
    , "\x1F971" -- Yawning Face
    , "\x1F62A" -- Sleepy Face
    , "\x1F634" -- Sleeping Face
    , "\x1F7E0" -- Orange Circle
    , "\x1F535" -- Blue Circle
    ]   

emojiVariations = [e <> m | e <- emoji, m <- ["", "\xFE0F", "\xFE0E"]]

chunksOf :: Int -> [e] -> [[e]]
chunksOf n = unfoldr (\l -> if length l > 0 then Just (take n l, drop n l) else Nothing)

showEmoji = [
    l
    | eLine <- chunksOf 30 emojiVariations
    , l <- [
        unwords [e <> "|" | e <- eLine],
        unwords [replicate (wcswidth e) '.' <> "|" | e <- eLine]
      ]
    ]

introText = vertCat $
    string (defAttr `withForeColor` black `withBackColor` green)
                      "Press ESC to exit. Events for keys below."
    : map (string defAttr) (
    [ "This demo shows various emoji."
    , "It also shows what vty thinks the width of every emoji is in terminal columns:"
    ] <> showEmoji)

updateDisplay :: App ()
updateDisplay = do
    vty <- ask
    liftIO $ update vty $ picForImage introText

handleNextEvent = ask >>= liftIO . nextEvent >>= handleEvent
    where
        handleEvent e = return $ e == EvKey KEsc []

