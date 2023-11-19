module Main (main) where

import Brick (Widget, joinBorders, simpleMain, str, withBorderStyle, (<+>))
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Lib

ui :: Widget ()
ui =
  joinBorders $
    withBorderStyle unicode $
      borderWithLabel (str "Hello!") $
        (center (str "Left") <+> vBorder <+> center (str "Right"))

main :: IO ()
main = simpleMain ui
