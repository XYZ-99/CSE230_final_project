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

import Brick.BChan (newBChan, writeBChan, BChan)
import Brick.Main (customMain)


import Control.Concurrent (forkIO, threadDelay, MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (forever, void)
import Databus (Databus(global_asciiart))
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as Vty




-- Placeholder for the video window
videoWindow :: Databus -> Int -> Widget ()
videoWindow db width = withBorderStyle unicode $
    borderWithLabel (str "Video Window") $
    hLimit width $ vBox [ str $ matrix_to_string (global_asciiart db) ]

-- Placeholder for the play/pause button
playPauseButton :: String -> Int -> Widget ()
playPauseButton play_or_pause height = withBorderStyle unicode $
    border $ vLimit height  $ str (if play_or_pause == "play" then "▶" else "■")

-- Placeholder for the progress bar
progressBar :: String -> Int -> Widget ()
progressBar progress height = withBorderStyle unicode $
    border $ vLimit height $ hBox [ str progress ]



drawUI :: Databus -> [Widget ()]
drawUI db = [mainInterface (ui2playbacklogic_status db) db]
  where
    mainInterface play_str databus = vBox [ videoWindow databus 50
                                           , hBox [ playPauseButton play_str 3
                                                  , progressBar "c" 3 ]
                                           ]


-- appEvent :: Databus -> BrickEvent () e -> EventM () (Next Databus)
-- appEvent db (VtyEvent (EvKey (KChar ' ') [])) =
--     continue $ if ui2playbacklogic_status db == "play" then db { ui2playbacklogic_status = "pause" } else db { ui2playbacklogic_status = "play" }
-- appEvent databus (VtyEvent (EvKey KEsc [])) =
--     halt databus
-- appEvent state _ = continue state

data CustomEvent = CustomRedrawEvent


appEvent :: Databus -> BrickEvent () CustomEvent -> EventM () (Next Databus)
appEvent db (AppEvent CustomRedrawEvent) = do
    -- Read the latest state from the MVar and update the UI
    updatedDb <- liftIO $ readMVar databusVar
    continue updatedDb
appEvent db (VtyEvent (EvKey (KChar ' ') [])) =
    continue $ if ui2playbacklogic_status db == "play" then db { ui2playbacklogic_status = "pause" } else db { ui2playbacklogic_status = "play" }
appEvent db (VtyEvent (EvKey KEsc [])) =
    halt db
appEvent db _ = continue db


-- uiapp :: App Databus e ()
-- uiapp = App { appDraw = drawUI
--           , appHandleEvent = appEvent
--           , appStartEvent = return
--           , appAttrMap = const theMap
--           , appChooseCursor = neverShowCursor
--           }
uiapp :: App Databus CustomEvent ()
uiapp = App { appDraw = drawUI
            , appHandleEvent = appEvent
            , appStartEvent = return
            , appAttrMap = const theMap
            , appChooseCursor = neverShowCursor
            }

theMap :: AttrMap
theMap = attrMap defAttr []

initialDatabus :: Databus
initialDatabus = MakeDatabus "" "" 0 0 ["studd","stuff"] "paused"


-- ui_main :: IO ()
-- ui_main = simpleMain $ mainInterface 3 50
-- ui_main :: IO ()
-- ui_main = do
--         defaultMain uiapp $ initialDatabus
--         return ()


matrix_to_string :: [[Char]] -> String
matrix_to_string matrix = foldl (\acc x -> acc ++ x ++ "\n") "" matrix

-- string_to_matrix :: String -> [[Char]]
-- string_to_matrix str = foldl (\acc x -> acc ++ [x]) [] str

get_asciiart :: IO [[Char]]
get_asciiart = do
    -- ... Fetch or generate ASCII art
    return ["New ASCII Art", "Line 2", "Line 3"]

startAsciiArtUpdater :: MVar Databus -> BChan CustomEvent -> IO ()
startAsciiArtUpdater databusVar chan = void $ forkIO $ forever $ do
    art <- get_asciiart
    modifyMVar_ databusVar (\db -> return db { global_asciiart = art })
    writeBChan chan CustomRedrawEvent  -- Custom event to trigger redraw
    threadDelay 1000000

ui_main :: IO ()
ui_main = do
    databusVar <- newMVar initialDatabus
    startAsciiArtUpdater databusVar
    chan <- newBChan 10  -- Create a channel for Brick events
    let buildVty = mkVty defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) uiapp databusVar


