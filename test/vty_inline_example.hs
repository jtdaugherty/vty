import Graphics.Vty
import Graphics.Vty.Inline

main = do
    t <- terminal_handle
    putStr "Not styled. "
    put_attr_change t $ back_color red >> apply_style underline
    putStr " Styled! "
    put_attr_change t $ default_all
    putStrLn "Not styled."
    release_terminal t
    return ()
