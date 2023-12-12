module UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.ProgressBar
import Graphics.Vty.Attributes
import Brick.Widgets.Border.Style
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Databus
import Control.Concurrent (forkIO, threadDelay)
import Brick.BChan
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)

import Graphics.Vty
import PlaybackLogic



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

appEvent :: Databus -> BrickEvent n Tick -> EventM n (Next Databus)
appEvent db (AppEvent Tick) = do
    newDb <- liftIO $ get_asciiart db
    continue newDb
appEvent db (VtyEvent (EvKey (KChar ' ') [])) = do
    let newStatus = if ui2playbacklogic_status db == "play" then "pause" else "play"
    newDb <- liftIO $ get_asciiart db { ui2playbacklogic_status = newStatus }
    continue newDb
appEvent db (VtyEvent (EvKey KEsc [])) = halt db
appEvent db _ = continue db


app :: App Databus Tick ()
app = App { appDraw = drawUI
          , appHandleEvent = appEvent
          , appStartEvent = return
          , appAttrMap = const $ attrMap defAttr []
          , appChooseCursor = neverShowCursor
          }


-- get_asciiart :: Databus -> IO Databus
-- get_asciiart db =do
--     let nextFrame = (global_current_frame db + 1) `mod` global_total_frames db
--      in db { global_current_frame = nextFrame, global_asciiart = ["Frame: " ++ show nextFrame] }
    
get_asciiart :: Databus -> IO Databus
get_asciiart = playbacklogic_main

data Tick = Tick


-- data Databus = MakeDatabus
--   { global_video_path :: String,
--     global_cache_path :: String,
--     global_current_frame :: Int,
--     global_total_frames :: Int,
--     global_asciiart :: [[Char]],

--     ui2playbacklogic_status :: String
--   }

db_global_video_path = "/Users/yifeichen/WorkSpace/GithubRepos/CSE230_final_project/CSE230_final_project/sample2.mov"
db_global_cache_path = "/Users/yifeichen/WorkSpace/GithubRepos/CSE230_final_project/CSE230_final_project/hascii-player/app/hascii-player-cache"


ui_main :: IO ()
ui_main = do
    let initialDatabus = MakeDatabus db_global_video_path db_global_cache_path 0 100 [] "play"
    chan <- newBChan 10
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 1000000  -- 1 second
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app initialDatabus