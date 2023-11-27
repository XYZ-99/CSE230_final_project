module Databus where

data Databus = MakeDatabus
  { global_video_path :: String,
    global_cache_path :: String,
    global_current_frame :: Int,
    global_total_frames :: Int,
    gloab_asciiart :: [[Char]],

    ui2playbacklogic_status :: String
  }