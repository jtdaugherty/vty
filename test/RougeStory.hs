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
    empty_aspects :: repr Aspects
    add_known_history :: repr History -> repr Aspects -> repr Aspects
    add_known_obj :: repr Object -> repr Aspects -> repr Aspects
    add_unknown_history :: repr History -> repr Aspects -> repr Aspects
    add_unknown_obj :: repr Object -> repr Aspects -> repr Aspects

    -- Aspects are animate objects, inanimate objects or histories.
    animate_obj :: Type -> repr Object
    inanimate_obj :: Type -> repr Object
    history :: repr History

    -- The goal is to acquire, or dispose of an object.
    dispose_obj :: repr Object -> repr Goal
    acquire_obj :: repr Object -> repr Goal

    -- What is an object? Well, that's anything that fits in the story language.
    -- Which at least means it's something that can be named
    named :: repr Object -> Name -> repr Object

data Quest
data History
data Goal
data Aspects
data Object

story_0 = 
    let bob = animate_obj "Human" `named` "Bob"
        the_quest = quest (dispose_obj (inanimate_obj "YoYo" `named` "The Kitten Slayer"))
                          (add_known_obj bob empty_aspects)
    in story "Bob" the_quest

{- TEMPLATE
instance Story X where
    story c q =
    quest g as =
    empty_aspects =
    add_known_history h as =
    add_known_obj obj as =
    add_unknown_history h as =
    add_unknown_obj obj as =
    animate_obj t =
    inanimate_obj t =
    history =
    dispose_obj obj =
    acquire_obj obj =
    named obj n =
-}

newtype ShowStory t = ShowStory { unShowStory :: String }
instance Story ShowStory where
    story c q = ShowStory $ printf "History(%s,%s)" c (unShowStory q)
    quest g as = ShowStory $ printf "Quest(%s,%s)" (unShowStory g) (unShowStory as)
    empty_aspects = ShowStory $ printf "EmptyAspects"
    add_known_history h as = ShowStory $ printf "Known(%s):%s" (unShowStory h) (unShowStory as)
    add_known_obj obj as = ShowStory $ printf "Known(%s):%s" (unShowStory obj) (unShowStory as)
    add_unknown_history h as = ShowStory $ printf "Unknown(%s):%s" (unShowStory h) (unShowStory as)
    add_unknown_obj obj as = ShowStory $ printf "Unknown(%s):%s" (unShowStory obj) (unShowStory as)
    animate_obj t = ShowStory $ printf "%s :: AnimateObj" t
    inanimate_obj t = ShowStory $ printf "%s :: InanimateObj" t
    history = ShowStory $ printf "History"
    dispose_obj obj = ShowStory $ printf "Goal(%s)" (unShowStory obj)
    acquire_obj obj = ShowStory $ printf "Acquire(%s)" (unShowStory obj)
    named obj n = ShowStory $ printf "%s+Name(%s)" (unShowStory obj) (show n)

newtype TellStory t = TellStory { unTellStory :: String }

instance Story TellStory where
    story c (TellStory quest) = TellStory $ "The story of " ++ c ++ " on the quest " ++ quest
    quest (TellStory g) (TellStory as)
        | as == ""  = TellStory $ g ++ "."
        | otherwise = TellStory $ g ++ ".  In a world where... " ++ as
    empty_aspects = TellStory ""
    add_known_history (TellStory h) (TellStory as)
        = TellStory $ "Our hero knows the tale of\n\t" ++ h ++ ".\n" ++ as
    add_known_obj (TellStory obj) (TellStory as)
        = TellStory $ "Our hero knows of " ++ obj ++ ".  " ++ as
    add_unknown_history _ as = as
    add_unknown_obj _ as = as
    animate_obj t = TellStory t
    inanimate_obj t = TellStory t
    history = TellStory "History"
    dispose_obj (TellStory obj)
        = TellStory $ "to dispose of the accursed " ++ obj
    acquire_obj (TellStory obj)
        = TellStory $ "to acquire the great " ++ obj
    named (TellStory obj) name
        = TellStory $ obj ++ " with the name " ++ name

newtype BuildSprites m t = BuildSprites {build_sprites :: StateT SpriteDB m ()}

type Sprite = (Int, Int, Image)
type Collider = (Int,Int,Int,Int)
data SpriteDB = SpriteDB
    { next_sprite_ID :: Int -- globally monotonic. H: Can fst . findMax + 1 be used?
    , this_sprite_ID :: Int
    , sprites        :: Map Int Sprite
    }
    deriving (Show)

get_sprite_ID :: MonadIO m => StateT SpriteDB m Int
get_sprite_ID = gets this_sprite_ID

new_sprite_ID :: MonadIO m => StateT SpriteDB m Int
new_sprite_ID = do
    ctx <- get
    let the_ID = next_sprite_ID ctx
    put $ ctx {next_sprite_ID = the_ID + 1, this_sprite_ID = the_ID}
    return the_ID

empty_sprite_db = SpriteDB 0 1 Map.empty

add_sprite x y img i
    = modify (\ctx -> ctx {sprites = Map.insert i (x,y,img) (sprites ctx)})

add_random_sprite c i = do
    let img = char def_attr c
    [x,y] <- liftIO $ replicateM 2 $ randomRIO (-100,100)
    add_sprite x y img i

instance MonadIO m => Story (BuildSprites m) where
    story _c (BuildSprites q) = BuildSprites q
    quest (BuildSprites g) (BuildSprites as) = BuildSprites (g >> as)
    empty_aspects = BuildSprites $ return ()
    add_known_history h (BuildSprites as)   = BuildSprites as
    add_known_obj obj (BuildSprites as)     = BuildSprites as
    add_unknown_history h (BuildSprites as) = BuildSprites as
    add_unknown_obj obj (BuildSprites as)   = BuildSprites as
    animate_obj   _t = BuildSprites $ do
        add_random_sprite '@' =<< new_sprite_ID
    inanimate_obj _t = BuildSprites $ do
        add_random_sprite 'X' =<< new_sprite_ID
    history         = BuildSprites $ return ()
    dispose_obj (BuildSprites obj)   = BuildSprites obj
    acquire_obj (BuildSprites obj)   = BuildSprites obj
    named       (BuildSprites obj) n = BuildSprites obj

render_sprites = foldMap render_sprite . sprites
render_sprite (x,y,img) = return $ translate x y img

main = do
    putStrLn $ unTellStory story_0
    putStrLn $ unShowStory story_0
    sprite_db <- execStateT (build_sprites story_0) empty_sprite_db
    print sprite_db
    let imgs = render_sprites sprite_db
        p = pic_for_layers imgs
    withVty $ flip update p
    threadDelay 5000000
    withVty $ shutdown
    return ()
