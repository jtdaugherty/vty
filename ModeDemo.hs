module Main where

import Data.Monoid

import Graphics.Vty

mkUI :: (Bool, Bool, Bool, Bool) -> Maybe Event -> Image
mkUI (m, ms, p, ps) e =
    vertCat [ string defAttr $ "Mouse mode supported: " <> show m
            , string defAttr $ "Mouse mode status: " <> show ms
            , string defAttr " "
            , string defAttr $ "Paste mode supported: " <> show p
            , string defAttr $ "Paste mode status: " <> show ps
            , string defAttr " "
            , string defAttr $ "Last event: " <> show e
            , string defAttr " "
            , string defAttr "Press 'm' to toggle mouse mode, 'p' to toggle paste mode, and 'q' to quit."
            ]

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg

    let renderUI lastE = do
          let output = outputIface vty
          info <- (,,,) <$> (pure $ supportsMode output Mouse)
                        <*> getModeStatus output Mouse
                        <*> (pure $ supportsMode output BracketedPaste)
                        <*> getModeStatus output BracketedPaste
          return $ picForImage $ mkUI info lastE

    let go lastE = do
          pic <- renderUI lastE
          update vty pic
          e <- nextEvent vty
          case e of
              EvKey (KChar 'q') [] -> return ()
              EvKey (KChar 'm') [] -> do
                  let output = outputIface vty
                  enabled <- getModeStatus output Mouse
                  setMode output Mouse (not enabled)
                  go (Just e)
              EvKey (KChar 'p') [] -> do
                  let output = outputIface vty
                  enabled <- getModeStatus output BracketedPaste
                  setMode output BracketedPaste (not enabled)
                  go (Just e)
              _ -> go (Just e)

    go Nothing
    shutdown vty
