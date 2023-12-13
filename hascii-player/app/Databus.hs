
module Databus where

import Graphics.Vty

data Databus = MakeDatabus
  { global_video_path :: String,
    global_cache_path :: String,
    global_current_frame :: Int,
    global_total_frames :: Int,
    global_asciiart :: [[Char]],
    global_width :: Int,
    global_vty :: Vty,

    global_subtitle :: String,
    global_video_length_seconds :: Int,
    global_subtitle_path :: String,

    ui2playbacklogic_status :: String
  }