module Databus where

data Databus = Databus
  { global_video_path :: String,
    global_cache_path :: String,
    global_current_frame :: Int,
    global_total_frames :: Int,
    global_asciiart :: [[Char]],

    ui2playbacklogic_status :: String
  }