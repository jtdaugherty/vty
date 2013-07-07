import Graphics.Vty
import Graphics.Vty.Inline

main = do
    putStr "Not styled. "
    put_attr_change_ t $ back_color red >> apply_style underline
    putStr " Styled! "
    put_attr_change_ t $ default_all
    putStrLn "Not styled."
