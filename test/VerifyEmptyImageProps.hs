{-# LANGUAGE ScopedTypeVariables #-}
module VerifyEmptyImageProps where

import Verify

-- should be exported by Graphics.Vty.Picture
import Graphics.Vty.Picture ( Image, empty_image )

tests :: IO [Test]
tests = do
    -- should provide an image type.
    let _ :: Image = empty_image
    return []

