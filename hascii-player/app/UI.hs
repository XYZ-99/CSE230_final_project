-- module UI where
module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Core
import Graphics.Vty.Attributes


-- Placeholder for the video window
videoWindow :: String -> Widget n
videoWindow videoContent = withBorderStyle unicode $
    borderWithLabel (str "Video Window") $
    vBox [ str videoContent ]

-- Placeholder for the play/pause button
playPauseButton :: String -> Widget n
playPauseButton buttonText = withBorderStyle unicode $
    border $ padAll 1 $ str buttonText

-- Placeholder for the progress bar
progressBar :: String -> Widget n
progressBar progress = withBorderStyle unicode $
    border $ hBox [ str progress ]


drawUI :: String -> String -> String -> [Widget n]
drawUI videoContent buttonText progress = [ui]
    where
        ui = vBox [ videoWindow videoContent
                  , hBox [ playPauseButton buttonText
                         , progressBar progress
                         ]
                  ]


main :: IO ()
main = defaultMain app "Some Video Content"
    where
        app = App { appDraw = drawUI "Some Video Content" "Play/Pause" "Progress"
                  , appChooseCursor = neverShowCursor
                  , appHandleEvent = handleEvent
                  , appStartEvent = return
                  , appAttrMap = const $ attrMap defAttr []
                  }
        handleEvent :: String -> BrickEvent n e -> EventM n (Next String)
        handleEvent s _ = continue s
