module PlaybackLogic where

import Brick


playbacklogic_main :: Databus -> Databus
playbacklogic_main databus = do
    let cache_dir               = global_cache_path databus,
        new_current_frame_index = playbacklogic_calc_current_frame databus,
        frame_path              = cache_dir ++ "/" ++ show (new_current_frame_index) ++ ".png",
    in
        maybeImage <- loadImage "example.png" targetWidth
        case maybeImage of
            Just image -> let new_asciiart = (T.unpack (toAsciiArt image)) in
                MakeDatabus { 
                    global_video_path       = global_video_path databus,
                    global_cache_path       = global_cache_path databus,
                    global_current_frame    = new_current_frame_index,
                    global_total_frames     = global_total_frames databus,
                    global_asciiart         = new_asciiart,
                    ui2playbacklogic_status = ui2playbacklogic_status databus,
                }
            Nothing -> MakeDatabus {
                global_video_path       = global_video_path databus,
                global_cache_path       = global_cache_path databus,
                global_current_frame    = global_current_frame databus,
                global_total_frames     = global_total_frames databus,
                global_asciiart         = global_asciiart databus,
                ui2playbacklogic_status = ui2playbacklogic_status databus,
            }

playbacklogic_calc_current_frame :: Databus -> Int
playbacklogic_calc_current_frame databus
  | ui2playbacklogic_status databus == "play"  = global_current_frame databus + 1
  | ui2playbacklogic_status databus == "pause" = global_current_frame databus
