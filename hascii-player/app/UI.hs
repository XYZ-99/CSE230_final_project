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
import System.Directory (getDirectoryContents, doesFileExist,doesDirectoryExist)
import System.Console.ANSI

import Graphics.Vty
import PlaybackLogic
import Databus (Databus(global_total_frames, global_asciiart))

import Preprocess(preprocess_video_to_frame, hash)

-- Video Window
videoWindow :: Databus -> Widget ()
videoWindow db = withBorderStyle unicode $
  borderWithLabel (str "Video Window") $
  vBox $ map str (global_asciiart db)

-- Play/Pause Button
playPauseButton :: Databus -> Widget ()
-- playPauseButton db = str $ "Status: " ++ ui2playbacklogic_status db
playPauseButton db = case ui2playbacklogic_status db of
    "play" -> str "⏸"
    "pause" -> str "▶️"
    _ -> str "■"

captionBar :: Databus -> Widget ()
captionBar db = str $ "\n" ++ setSGRCode [SetColor Foreground Vivid Green] ++ global_subtitile db ++ setSGRCode [Reset] ++ "\n"


-- -- Progress Bar
-- myProgressBar :: Databus -> Widget ()
-- myProgressBar db = 
--     let progressValue = fromIntegral (global_current_frame db) / fromIntegral (global_total_frames db)
--     in progressBar Nothing progressValue 

-- Replace the myProgressBar function with this:
myProgressBar :: Databus -> Widget ()
myProgressBar db =
    let width = 195
        progressValue = fromIntegral (global_current_frame db) / fromIntegral (global_total_frames db)
        progressLength = round (progressValue * fromIntegral width)
        progressBarText = playPauseButton db ++ setSGRCode [SetColor Foreground Vivid Red] ++ show (round (progressValue * 100)) ++ "[" ++ "%" ++ replicate progressLength '█' ++ replicate (width - progressLength) '.' ++ "]"
    in str $ progressBarText


drawUI :: Databus -> [Widget ()]
drawUI db = [uiLayout db]
  where
    uiLayout databus = vBox [ videoWindow databus
                            -- , playPauseButton databus
                            , captionBar databus
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
appEvent db (VtyEvent (EvKey KLeft [])) = do
    let new_db = MakeDatabus (global_video_path db) (global_cache_path db) (if global_current_frame db - 5 < 1 then 1 else global_current_frame db - 5) (global_total_frames db) (global_asciiart db) (ui2playbacklogic_status db)
    continue new_db
appEvent db (VtyEvent (EvKey KRight [])) = do
    let total_frames = global_total_frames db
    let new_db = MakeDatabus (global_video_path db) (global_cache_path db)  (if global_current_frame db + 5 > total_frames then total_frames else global_current_frame db + 5) (global_total_frames db) (global_asciiart db) (ui2playbacklogic_status db)
    continue new_db

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

-- db_global_video_path = "/Users/yifeichen/WorkSpace/GithubRepos/CSE230_final_project/CSE230_final_project/sample2.mov"
db_global_cache_path = "./hascii-player/app/hascii-player-cache"


-- string_of_zeros :: Int -> String
-- string_of_zeros 0 = ""
-- string_of_zeros n = "0" ++ (string_of_zeros (n-1)) 

-- align_length :: Int -> String
-- align_length frame_num = let frame_num_str = show frame_num
--                             in (string_of_zeros (8 - length frame_num_str)) ++ frame_num_str



get_frames_dir :: Databus -> String
get_frames_dir db = global_cache_path db ++ "/" ++ show (hash (global_video_path db)) ++ "/frames"

-- getDirectoryContents :: FilePath -> IO [FilePath]

interval = 100000

ui_main :: String -> IO ()
ui_main video_path = do
    
    let rawDatabus = MakeDatabus video_path db_global_cache_path 1 100 [] "play"
    cached <- doesDirectoryExist $ get_frames_dir  rawDatabus
    if cached then return () else preprocess_video_to_frame video_path db_global_cache_path
    frames <- getDirectoryContents $ get_frames_dir rawDatabus
    let initialDatabus = MakeDatabus video_path db_global_cache_path 1 ((length frames) - 2) [] "play"
    chan <- newBChan 10
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay interval 
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) app initialDatabus