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
    -- , render_single_column_char_op
    -- , render_double_column_char_op
    ]

idImageOp :: ImageOp
idImageOp = ImageOp id id

-- render_char_op :: ImageOp
-- render_char_op = ImageOp id id
