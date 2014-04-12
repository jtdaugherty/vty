module Graphics.Vty.Debug.Image where

import Graphics.Vty.Image

type ImageConstructLog = [ImageConstructEvent]
data ImageConstructEvent = ImageConstructEvent
    deriving ( Show, Eq )

forwardImageOps :: [Image -> Image]
forwardImageOps = map forwardTransform debugImageOps

forwardTransform, reverseTransform :: ImageOp -> (Image -> Image)

forwardTransform (ImageOp f _) = f
reverseTransform (ImageOp _ r) = r

data ImageOp = ImageOp ImageEndo ImageEndo
type ImageEndo = Image -> Image

debugImageOps :: [ImageOp]
debugImageOps = 
    [ idImageOp
    -- , renderSingleColumnCharOp
    -- , renderDoubleColumnCharOp
    ]

idImageOp :: ImageOp
idImageOp = ImageOp id id

-- renderCharOp :: ImageOp
-- renderCharOp = ImageOp id id
