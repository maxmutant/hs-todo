module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

ui :: Widget ()
ui =
    withBorderStyle unicode $
    borderWithLabel (str "Snake") (center (str "Hello"))

main :: IO ()
main = simpleMain ui
