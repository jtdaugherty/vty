module Verify.Graphics.Vty.Attributes ( module Verify.Graphics.Vty.Attributes
                                      , module Graphics.Vty.Attributes
                                      )
    where

import Graphics.Vty.Attributes
import Verify

import Data.List ( delete )

-- Limit the possible attributes to just a few for now.
possible_attr_mods :: [ AttrOp ]
possible_attr_mods = 
    [ set_bold_op
    , id_op 
    ]

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

set_bold_op = AttrOp "set_bold" (flip with_style bold)
id_op = AttrOp "id" id

apply_op :: AttrOp -> Attr -> Attr
apply_op (AttrOp _ f) a = f a

