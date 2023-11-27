module VideoProcessing where

import System.Process

-- Function to extract frames using FFmpeg
extractFrames :: FilePath -> FilePath -> IO ()
extractFrames videoPath outputPattern = do
    let command = "ffmpeg"
    let arguments = ["-i", videoPath, "-r", "1", "-f", "image2", outputPattern]
    callProcess command arguments