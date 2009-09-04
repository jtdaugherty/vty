{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.Vty

import qualified Data.ByteString as B
import Data.Word

import System.IO


main = do 
    vt <- mkVty
    DisplayRegion w h <- display_bounds $ terminal vt
    putStrLn $ show $ DisplayRegion w h
    play vt 0 1 w h ""

pieceA = def_attr `with_fore_color` red
dumpA = def_attr `with_style` reverse_video

play :: Vty -> Word -> Word -> Word -> Word -> String -> IO ()
play vt x y sx sy btl = do update vt (current_pic x y sx sy btl)
                           k <- next_event vt
                           case k of EvKey (KASCII 'r') [MCtrl]    -> refresh vt >> play vt x y sx sy btl
                                     EvKey KLeft  [] | x /= 0      -> play vt (x-1) y sx sy btl
                                     EvKey KRight [] | x /= (sx-1) -> play vt (x+1) y sx sy btl
                                     EvKey KUp    [] | y /= 1      -> play vt x (y-1) sx sy btl
                                     EvKey KDown  [] | y /= (sy-2) -> play vt x (y+1) sx sy btl
                                     EvKey KEsc   []               -> shutdown vt >> return ()
                                     EvResize nx ny                -> play vt (min x (toEnum nx - 1)) 
                                                                              (min y (toEnum ny - 2)) 
                                                                              (toEnum nx) 
                                                                              (toEnum ny) 
                                                                              btl
                                     _                             -> play vt x y sx sy (take (fromEnum sx) (show k ++ btl))


current_pic :: Word -> Word -> Word -> Word -> String -> Picture
current_pic x y sx sy btl = pic_for_image i
    where i =   string def_attr "Move the @ character around with the arrow keys. Escape exits."
            <-> char_fill pieceA ' ' sx (y - 1) 
            <-> char_fill pieceA ' ' x 1 <|> char pieceA '@' <|> char_fill pieceA ' ' (sx - x - 1) 1 
            <-> char_fill pieceA ' ' sx (sy - y - 2) 
            <-> iso_10464_string dumpA btl
