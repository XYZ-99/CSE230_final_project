module PlaybackLogic where

import Databus
import ASCIILoading
import qualified Data.Text as T

--  handle I/O operations and calling playbacklogic_main
updateDatabusWithImage :: Databus -> IO Databus
updateDatabusWithImage databus = do
    let frame_path = global_cache_path databus ++ "/" ++ show (global_current_frame databus) ++ ".png"
    let targetWidth = 800
    maybeImage <- loadImage frame_path targetWidth
    case maybeImage of
        Just image -> do
            let asciiArt = lines (T.unpack (toAsciiArt image))
            return $ playbacklogic_main databus asciiArt
        Nothing -> return databus

-- accepts ASCII art as a parameter and returns Databus
playbacklogic_main :: Databus -> [String] -> Databus
playbacklogic_main databus asciiArt =
    let new_current_frame_index = playbacklogic_calc_current_frame databus
    in MakeDatabus {
        global_video_path       = global_video_path databus,
        global_cache_path       = global_cache_path databus,
        global_current_frame    = new_current_frame_index,
        global_total_frames     = global_total_frames databus,
        global_asciiart         = asciiArt,
        ui2playbacklogic_status = ui2playbacklogic_status databus
    }

playbacklogic_calc_current_frame :: Databus -> Int
playbacklogic_calc_current_frame databus
  | ui2playbacklogic_status databus == "play"  = nextFrameIndex databus
  | otherwise = global_current_frame databus
  where
    nextFrameIndex db
      | currentFrame < totalFrames - 1 = currentFrame + 1
      | otherwise = totalFrames - 1
      -- otherwise = 0
      where
        currentFrame = global_current_frame db
        totalFrames = global_total_frames db
