module Verify.Graphics.Vty.Attributes ( module Verify.Graphics.Vty.Attributes
                                      , module Graphics.Vty.Attributes
                                      )
    where

import Graphics.Vty.Attributes
import Verify

import Data.List ( delete )

all_colors :: [Color]
all_colors =
    [ black
    , red
    , green
    , yellow
    , blue
    , magenta
    , cyan
    , white
    , bright_black
    , bright_red
    , bright_green
    , bright_yellow
    , bright_blue
    , bright_magenta
    , bright_cyan
    , bright_white
    ] ++ map Color240 [0..239]

all_styles :: [Style]
all_styles =
    [ standout
    , underline
    , reverse_video
    , blink
    , dim
    , bold
    ]

-- Limit the possible attributes to just a few for now.
possible_attr_mods :: [ AttrOp ]
possible_attr_mods = 
    [ id_op 
    ] ++ map set_fore_color_op all_colors
      ++ map set_back_color_op all_colors
      ++ map set_style_op all_styles

instance Arbitrary Attr where
    arbitrary = elements possible_attr_mods >>= return . flip apply_op def_attr

data DiffAttr = DiffAttr Attr Attr

instance Arbitrary DiffAttr where
    arbitrary = do
        op0 <- elements possible_attr_mods
        let possible_attr_mods' = delete op0 possible_attr_mods
        op1 <- elements possible_attr_mods'
        return $ DiffAttr (apply_op op0 def_attr) (apply_op op1 def_attr)

data AttrOp = AttrOp String (Attr -> Attr)

instance Eq AttrOp where
    AttrOp n0 _ == AttrOp n1 _ = n0 == n1

set_style_op s = AttrOp "set_style" (flip with_style s)
set_fore_color_op c = AttrOp "set_fore_color" (flip with_fore_color c)
set_back_color_op c = AttrOp "set_back_color" (flip with_back_color c)
id_op = AttrOp "id" id

apply_op :: AttrOp -> Attr -> Attr
apply_op (AttrOp _ f) a = f a

