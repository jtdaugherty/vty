{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Vty.Debug.Image where

import Graphics.Vty.Image

type ImageConstructLog = [ImageConstructEvent]
data ImageConstructEvent = ImageConstructEvent
    deriving ( Show, Eq )

forward_image_ops :: [Image -> Image]
forward_image_ops = map forward_transform debug_image_ops

forward_transform, reverse_transform :: ImageOp -> (Image -> Image)

forward_transform (ImageOp f _) = f
reverse_transform (ImageOp _ r) = r

data ImageOp = ImageOp ImageEndo ImageEndo
type ImageEndo = Image -> Image

debug_image_ops :: [ImageOp]
debug_image_ops = 
    [ id_image_op
    -- , render_single_column_char_op
    -- , render_double_column_char_op
    ]

id_image_op :: ImageOp
id_image_op = ImageOp id id

-- render_char_op :: ImageOp
-- render_char_op = ImageOp id id
