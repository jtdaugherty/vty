import Graphics.Vty
import Graphics.Vty.Inline

main = do
    putStr "Not styled. "
    putAttrChange_ t $ backColor red >> applyStyle underline
    putStr " Styled! "
    putAttrChange_ t $ defaultAll
    putStrLn "Not styled."
