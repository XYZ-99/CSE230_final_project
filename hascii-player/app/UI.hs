module UI where
-- module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.Border.Style (unicode, borderStyleFromChar)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Brick.Main(App, defaultMain)
import Databus





-- Placeholder for the video window
videoWindow :: String -> Int -> Widget ()
videoWindow videoContent width = withBorderStyle unicode $
    borderWithLabel (str "Video Window") $
    hLimit width $ vBox [ str videoContent ]

-- Placeholder for the play/pause button
playPauseButton :: String -> Int -> Widget ()
playPauseButton play_or_pause height = withBorderStyle unicode $
    border $ vLimit height  $ str (if play_or_pause == "play" then "▶" else "■")

-- Placeholder for the progress bar
progressBar :: String -> Int -> Widget ()
progressBar progress height = withBorderStyle unicode $
    border $ vLimit height $ hBox [ str progress ]



drawUI :: Databus -> [Widget ()]
drawUI db = [mainInterface (ui2playbacklogic_status db)]
  where
    mainInterface play_str = vBox [ videoWindow "a" 50
                                   , hBox [ playPauseButton play_str 3
                                          , progressBar "c" 3 ]
                                   ]


appEvent :: Databus -> BrickEvent () e -> EventM () (Next Databus)
appEvent db (VtyEvent (EvKey (KChar ' ') [])) =
    continue $ if ui2playbacklogic_status db == "play" then db { ui2playbacklogic_status = "pause" } else db { ui2playbacklogic_status = "play" }
appEvent databus (VtyEvent (EvKey KEsc [])) =
    halt databus
appEvent state _ = continue state

uiapp :: App Databus e ()
uiapp = App { appDraw = drawUI
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          , appChooseCursor = neverShowCursor
          }

theMap :: AttrMap
theMap = attrMap defAttr []

initialDatabus :: Databus
initialDatabus = MakeDatabus "" "" 0 0 [] "paused"


-- ui_main :: IO ()
-- ui_main = simpleMain $ mainInterface 3 50
ui_main :: IO ()
ui_main = do
        defaultMain uiapp $ initialDatabus
        return ()