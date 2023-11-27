module Databus where

data Databus = Databus
  { global_video_path :: String,
    global_cache_path :: String,
    global_current_frame :: Int,
    global_total_frames :: Int,
    ui2playbacklogic_status :: String,
    playbacklogic2asciiloading_asciiart :: [[Char]],
    asciiloading2playbacklogic_asciiart :: [[Char]]
  }