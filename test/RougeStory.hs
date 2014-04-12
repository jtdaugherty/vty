{-# LANGUAGE NoMonomorphismRestriction #-}
-- investigation using typed tagless interpretter of an AST representation of a story.
-- Use extensible effects instead?
module RougeStory where

import Text.Printf

import Graphics.Vty
import Graphics.Vty.Inline

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.Random

-- Characters are identified by Names
type Character = Name
type Name = String
type Type = String

-- The game is the view of a story.
class Story repr where
    -- A story is the quest of a character which becomes a history
    story :: Character -> repr Quest -> repr History
    quest :: repr Goal -> repr Aspects -> repr Quest

    -- A quest is for a goal where some aspects are known and others unknown.
    -- TODO: abstract to any Foldable?
    emptyAspects :: repr Aspects
    addKnownHistory :: repr History -> repr Aspects -> repr Aspects
    addKnownObj :: repr Object -> repr Aspects -> repr Aspects
    addUnknownHistory :: repr History -> repr Aspects -> repr Aspects
    addUnknownObj :: repr Object -> repr Aspects -> repr Aspects

    -- Aspects are animate objects, inanimate objects or histories.
    animateObj :: Type -> repr Object
    inanimateObj :: Type -> repr Object
    history :: repr History

    -- The goal is to acquire, or dispose of an object.
    disposeObj :: repr Object -> repr Goal
    acquireObj :: repr Object -> repr Goal

    -- What is an object? Well, that's anything that fits in the story language.
    -- Which at least means it's something that can be named
    named :: repr Object -> Name -> repr Object

data Quest
data History
data Goal
data Aspects
data Object

story0 = 
    let bob = animateObj "Human" `named` "Bob"
        theQuest = quest (disposeObj (inanimateObj "YoYo" `named` "The Kitten Slayer"))
                         (addKnownObj bob emptyAspects)
    in story "Bob" theQuest

{- TEMPLATE
instance Story X where
    story c q =
    quest g as =
    emptyAspects =
    addKnownHistory h as =
    addKnownObj obj as =
    addUnknownHistory h as =
    addUnknownObj obj as =
    animateObj t =
    inanimateObj t =
    history =
    disposeObj obj =
    acquireObj obj =
    named obj n =
-}

newtype ShowStory t = ShowStory { unShowStory :: String }
instance Story ShowStory where
    story c q = ShowStory $ printf "History(%s,%s)" c (unShowStory q)
    quest g as = ShowStory $ printf "Quest(%s,%s)" (unShowStory g) (unShowStory as)
    emptyAspects = ShowStory $ printf "EmptyAspects"
    addKnownHistory h as = ShowStory $ printf "Known(%s):%s" (unShowStory h) (unShowStory as)
    addKnownObj obj as = ShowStory $ printf "Known(%s):%s" (unShowStory obj) (unShowStory as)
    addUnknownHistory h as = ShowStory $ printf "Unknown(%s):%s" (unShowStory h) (unShowStory as)
    addUnknownObj obj as = ShowStory $ printf "Unknown(%s):%s" (unShowStory obj) (unShowStory as)
    animateObj t = ShowStory $ printf "%s :: AnimateObj" t
    inanimateObj t = ShowStory $ printf "%s :: InanimateObj" t
    history = ShowStory $ printf "History"
    disposeObj obj = ShowStory $ printf "Goal(%s)" (unShowStory obj)
    acquireObj obj = ShowStory $ printf "Acquire(%s)" (unShowStory obj)
    named obj n = ShowStory $ printf "%s+Name(%s)" (unShowStory obj) (show n)

newtype TellStory t = TellStory { unTellStory :: String }

instance Story TellStory where
    story c (TellStory quest) = TellStory $ "The story of " ++ c ++ " on the quest " ++ quest
    quest (TellStory g) (TellStory as)
        | as == ""  = TellStory $ g ++ "."
        | otherwise = TellStory $ g ++ ".  In a world where... " ++ as
    emptyAspects = TellStory ""
    addKnownHistory (TellStory h) (TellStory as)
        = TellStory $ "Our hero knows the tale of\n\t" ++ h ++ ".\n" ++ as
    addKnownObj (TellStory obj) (TellStory as)
        = TellStory $ "Our hero knows of " ++ obj ++ ".  " ++ as
    addUnknownHistory _ as = as
    addUnknownObj _ as = as
    animateObj t = TellStory t
    inanimateObj t = TellStory t
    history = TellStory "History"
    disposeObj (TellStory obj)
        = TellStory $ "to dispose of the accursed " ++ obj
    acquireObj (TellStory obj)
        = TellStory $ "to acquire the great " ++ obj
    named (TellStory obj) name
        = TellStory $ obj ++ " with the name " ++ name

newtype BuildSprites m t = BuildSprites {buildSprites :: StateT SpriteDB m ()}

type Sprite = (Int, Int, Image)
type Collider = (Int,Int,Int,Int)
data SpriteDB = SpriteDB
    { nextSpriteID :: Int -- globally monotonic. H: Can fst . findMax + 1 be used?
    , thisSpriteID :: Int
    , sprites      :: Map Int Sprite
    }
    deriving (Show)

getSpriteID :: MonadIO m => StateT SpriteDB m Int
getSpriteID = gets thisSpriteID

newSpriteID :: MonadIO m => StateT SpriteDB m Int
newSpriteID = do
    ctx <- get
    let the_ID = nextSpriteID ctx
    put $ ctx {nextSpriteID = the_ID + 1, thisSpriteID = the_ID}
    return the_ID

emptySpriteDb = SpriteDB 0 1 Map.empty

addSprite x y img i
    = modify (\ctx -> ctx {sprites = Map.insert i (x,y,img) (sprites ctx)})

addRandomSprite c i = do
    let img = char def_attr c
    [x,y] <- liftIO $ replicateM 2 $ randomRIO (-100,100)
    addSprite x y img i

instance MonadIO m => Story (BuildSprites m) where
    story _c (BuildSprites q) = BuildSprites q
    quest (BuildSprites g) (BuildSprites as) = BuildSprites (g >> as)
    emptyAspects = BuildSprites $ return ()
    addKnownHistory h (BuildSprites as)   = BuildSprites as
    addKnownObj obj (BuildSprites as)     = BuildSprites as
    addUnknownHistory h (BuildSprites as) = BuildSprites as
    addUnknownObj obj (BuildSprites as)   = BuildSprites as
    animateObj   _t = BuildSprites $ do
        addRandomSprite '@' =<< newSpriteID
    inanimateObj _t = BuildSprites $ do
        addRandomSprite 'X' =<< newSpriteID
    history         = BuildSprites $ return ()
    disposeObj (BuildSprites obj)   = BuildSprites obj
    acquireObj (BuildSprites obj)   = BuildSprites obj
    named       (BuildSprites obj) n = BuildSprites obj

renderSprites = foldMap renderSprite . sprites
renderSprite (x,y,img) = return $ translate x y img

main = do
    putStrLn $ unTellStory story0
    putStrLn $ unShowStory story0
    spriteDb <- execStateT (buildSprites story0) emptySpriteDb
    print spriteDb
    let imgs = renderSprites spriteDb
        p = pic_for_layers imgs
    withVty $ flip update p
    threadDelay 5000000
    withVty $ shutdown
    return ()
