module Main where

import VideoProcessing

main :: IO ()
main = do
    let videoPath = "media/videos/movie.mp4"
    let outputPattern = "media/frames/frame-%04d.png"  -- Output directory and pattern
    extractFrames videoPath outputPattern
    putStrLn "Frame extraction complete."

