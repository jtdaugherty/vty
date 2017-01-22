module Verify.Graphics.Vty.Attributes
  ( module Verify.Graphics.Vty.Attributes
  , module Graphics.Vty.Attributes
  )
where

import Graphics.Vty.Attributes
import Verify

import Data.List ( delete )

allColors :: [Color]
allColors =
    [ black
    , red
    , green
    , yellow
    , blue
    , magenta
    , cyan
    , white
    , brightBlack
    , brightRed
    , brightGreen
    , brightYellow
    , brightBlue
    , brightMagenta
    , brightCyan
    , brightWhite
    ] ++ map Color240 [0..239]

allStyles :: [Style]
allStyles =
    [ standout
    , underline
    , reverseVideo
    , blink
    , dim
    , bold
    ]

-- Limit the possible attributes to just a few for now.
possibleAttrMods :: [ AttrOp ]
possibleAttrMods =
    [ idOp
    ] ++ map setForeColorOp allColors
      ++ map setBackColorOp allColors
      ++ map setStyleOp allStyles

instance Arbitrary Attr where
    arbitrary = elements possibleAttrMods >>= return . flip applyOp defAttr

data DiffAttr = DiffAttr Attr Attr

instance Arbitrary DiffAttr where
    arbitrary = do
        op0 <- elements possibleAttrMods
        let possibleAttrMods' = delete op0 possibleAttrMods
        op1 <- elements possibleAttrMods'
        return $ DiffAttr (applyOp op0 defAttr) (applyOp op1 defAttr)

data AttrOp = AttrOp String (Attr -> Attr)

instance Eq AttrOp where
    AttrOp n0 _ == AttrOp n1 _ = n0 == n1

setStyleOp s     = AttrOp "set_style"      (flip withStyle s)
setForeColorOp c = AttrOp "set_fore_color" (flip withForeColor c)
setBackColorOp c = AttrOp "set_back_color" (flip withBackColor c)
idOp = AttrOp "id" id

applyOp :: AttrOp -> Attr -> Attr
applyOp (AttrOp _ f) a = f a
