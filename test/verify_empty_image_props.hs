{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Verify

-- should be exported by Graphics.Vty.Picture
import Graphics.Vty.Picture ( Image, empty_image )

main = run_test $ do
    -- should provide an image type.
    let _ :: Image = empty_image
    return ()

