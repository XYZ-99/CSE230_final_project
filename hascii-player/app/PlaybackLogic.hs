{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module PlaybackLogic where

import Databus
import ASCIILoading
import qualified Data.Text as T
import Preprocess (hash)
import Databus (Databus(global_video_path))



--  main
playbacklogic_main :: Databus -> IO Databus
playbacklogic_main databus = do
    let framePath = constructFramePath databus
    let targetWidth = 100  -- TODO
    maybeImage <- loadImage framePath targetWidth
    case maybeImage of
        Just image -> do
            let asciiArt = lines (T.unpack (toAsciiArt image))
            return $ updateDatabus databus asciiArt
        Nothing -> do
            putStrLn ("Error: Failed to load image." ++ constructFramePath databus )
            return databus


string_of_zeros :: Int -> String
string_of_zeros 0 = ""
string_of_zeros n = "0" ++ (string_of_zeros (n-1)) 

align_length :: Int -> String
align_length frame_num = let frame_num_str = show frame_num
                            in (string_of_zeros (8 - length frame_num_str)) ++ frame_num_str


-- Construct frame path
constructFramePath :: Databus -> String
constructFramePath databus = global_cache_path databus ++ "/" ++ show (hash (global_video_path databus)) ++ "/frames/" ++ align_length (global_current_frame databus) ++ ".jpeg"

-- Construct Databus
constructDatabus :: Databus -> [String] -> Int -> Databus
constructDatabus databus asciiArt newCurrentFrameIndex = MakeDatabus {
    global_video_path       = global_video_path databus,
    global_cache_path       = global_cache_path databus,
    global_current_frame    = newCurrentFrameIndex,
    global_total_frames     = global_total_frames databus,
    global_asciiart         = asciiArt,
    ui2playbacklogic_status = ui2playbacklogic_status databus
}

-- Update and return Databus
updateDatabus :: Databus -> [String] -> Databus
updateDatabus databus asciiArt =
    let newCurrentFrameIndex = playbacklogic_calc_current_frame databus
    in constructDatabus databus asciiArt newCurrentFrameIndex

playbacklogic_calc_current_frame :: Databus -> Int
playbacklogic_calc_current_frame databus
  | status == "play"  = if currentFrame < totalFrames - 1 then nextFrameIndex else totalFrames - 1
  | status == "fast-forward" = fastForwardIndex 
  | status == "rewind" = rewindIndex 
  | status == "pause" = currentFrame
  | status == "end" = totalFrames - 1
  | status == "beginning" = 0
  | otherwise = global_current_frame databus
  where
    status = ui2playbacklogic_status databus
    nextFrameIndex 
      | currentFrame < totalFrames - 1 = currentFrame + 1
      | otherwise = totalFrames - 1
    fastForwardIndex 
      | currentFrame + 10 < totalFrames = currentFrame + 10
      | otherwise = totalFrames - 1
    rewindIndex 
      | currentFrame - 10 >= 0 = currentFrame - 10
      | otherwise = 0
    currentFrame = global_current_frame databus
    totalFrames = global_total_frames databus


