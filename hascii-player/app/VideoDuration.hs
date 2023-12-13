module VideoDuration where

import Codec.FFmpeg.Probe (getDuration)
import Control.Exception (handle, SomeException)
import Databus (Databus(global_video_path))

getVideoDuration :: Databus -> IO (Either String Double)
getVideoDuration databus = do
    let videoPath = global_video_path databus
    handle errorHandler (getDuration videoPath)
  where
    errorHandler :: SomeException -> IO (Either String Double)
    errorHandler _ = return $ Left "Error: Failed to get video duration."

