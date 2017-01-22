module VerifyEmptyImageProps where

import Verify

-- should be exported by Graphics.Vty.Picture
import Graphics.Vty.Image ( Image, emptyImage )

tests :: IO [Test]
tests = do
    -- should provide an image type.
    let _ :: Image = emptyImage
    return []

