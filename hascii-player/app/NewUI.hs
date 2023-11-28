module NewUI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.ProgressBar
import Graphics.Vty.Attributes
import Brick.Widgets.Border.Style
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Databus

-- Video Window
videoWindow :: Databus -> Widget ()
videoWindow db = withBorderStyle unicode $
  borderWithLabel (str "Video Window") $
  vBox $ map str (global_asciiart db)

-- Play/Pause Button
playPauseButton :: Databus -> Widget ()
playPauseButton db = str $ "Status: " ++ ui2playbacklogic_status db

-- Progress Bar
myProgressBar :: Databus -> Widget ()
myProgressBar db = 
    let progressValue = fromIntegral (global_current_frame db) / fromIntegral (global_total_frames db)
    in progressBar Nothing progressValue 

drawUI :: Databus -> [Widget ()]
drawUI db = [uiLayout db]
  where
    uiLayout databus = vBox [ videoWindow databus
                            , playPauseButton databus
                            , myProgressBar databus
                            ]
appEvent :: Databus -> BrickEvent n e -> EventM n (Next Databus)
appEvent db (VtyEvent (EvKey (KChar ' ') [])) = do
    let newStatus = if ui2playbacklogic_status db == "play" then "pause" else "play"
    let updatedDb = db { ui2playbacklogic_status = newStatus }
    continue $ get_asciiart updatedDb  -- Update ASCII art based on new status
appEvent db (VtyEvent (EvKey KEsc [])) = halt db
appEvent db _ = continue db

app :: App Databus e ()
app = App { appDraw = drawUI
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const $ attrMap defAttr []
          , appChooseCursor = neverShowCursor
          }

get_asciiart :: Databus -> Databus
get_asciiart db =
  case ui2playbacklogic_status db of
    "play" -> db { global_asciiart = ["Playing", "..."] }  -- Replace with actual ASCII art for playing
    "pause" -> db { global_asciiart = ["Pausing", "..."] } -- Replace with actual ASCII art for pausing
    _ -> db  -- Default case, can be modified as needed


get_databus :: Databus -> IO Databus
get_databus db = do
  let nextFrame = (global_current_frame db + 1) `mod` global_total_frames db
  let dbWithNextFrame = db { global_current_frame = nextFrame }
  return $ get_asciiart dbWithNextFrame


-- new_ui_main :: IO ()
-- new_ui_main = do
--    let initialDatabus = MakeDatabus "" "" 0 100 ["Initial", "State"] "play"
--    let loop db = do
--         db' <- get_databus db
--         defaultMain app db'
--         loop db'
--    loop initialDatabus

new_ui_main :: IO ()
new_ui_main = do
   let initialDatabus = MakeDatabus "" "" 0 100 ["Initial", "State"] "play"
   defaultMain app $ get_asciiart initialDatabus
   return ()