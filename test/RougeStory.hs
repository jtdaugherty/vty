{-# LANGUAGE NoMonomorphismRestriction #-}
module RougeStory where

import Text.Printf

import Graphics.Vty
import Graphics.Vty.Inline

import Control.Monad.Trans.State.Strict

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

newtype BuildPicture t = BuildPicture {build_picture :: Picture -> State PictureCtx Picture}

type Sprite = (Int, Int, Image)
data PictureCtx = PictureCtx
    { next_ID          :: Int -- globally monotonic
    , this_ID          :: Int
    , known_sprites    :: Map Int Sprite
    }

get_ID :: State PictureCtx Int
get_ID = gets this_ID

new_ID :: Picture -> State PictureCtx Picture
new_ID p = do
    ctx <- get
    let the_ID = next_ID ctx
    put $ ctx {next_ID = the_ID + 1, this_ID = the_ID}
    return p

empty_picture_ctx = PictureCtx 0 1 Map.empty

instance Story BuildPicture where
    story _c (BuildPicture q) = BuildPicture q
    quest _g (BuildPicture as) = BuildPicture as
    empty_aspects = BuildPicture new_ID
    add_known_history h (BuildPicture as) = BuildPicture as
    add_known_obj obj (BuildPicture as) = BuildPicture as
    add_unknown_history h (BuildPicture as) = BuildPicture as
    add_unknown_obj obj (BuildPicture as) = BuildPicture as
    animate_obj t = BuildPicture new_ID
    inanimate_obj t = BuildPicture new_ID
    history = BuildPicture new_ID
    dispose_obj (BuildPicture obj) = BuildPicture obj
    acquire_obj (BuildPicture obj) = BuildPicture obj
    named (BuildPicture obj) n = BuildPicture obj

main = do
    putStrLn $ unTellStory story_0
    putStrLn $ unShowStory story_0
    withVty $ flip update $ evalState (build_picture story_0 empty_picture) empty_picture_ctx
