module Subtitle where


import System.IO
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Data.List (find)
import Control.Exception 
import Data.Time
import Text.Read



import Databus
import Preprocess(hash)


maybe_to_string :: Maybe String -> String
maybe_to_string (Just a) = a
maybe_to_string Nothing = ""



get_srt_file_path :: Databus -> String
get_srt_file_path db = global_subtitle_path db


-- Define a data structure for a subtitle entry
data Subtitle = Subtitle {
    startTime :: NominalDiffTime,
    endTime   :: NominalDiffTime,
    text      :: String
} deriving (Show)

-- Function to parse time format
parseTime :: String -> NominalDiffTime
parseTime s = let (h:m:rest:_) = splitWhen (== ':') s
                  [secs, msStr] = splitWhen (== ',') rest
              in fromIntegral ((read h :: Int) * 3600 + (read m :: Int) * 60 + (read secs :: Int)) + (fromIntegral (read msStr :: Int) / 1000)

-- Helper function to split strings
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitWhen p s''
                            where (w, s'') = break p s'

-- Function to parse an SRT file
parseSRT :: String -> [Subtitle]
parseSRT contents = 
    let lns = lines contents
        blocks = splitWhen null lns
    in map parseBlock blocks
  where
    parseBlock (n:startEnd:txt) = 
        let [start, _, end] = map parseTime $ splitWhen (== ' ') startEnd
        in Subtitle start end (unlines txt)

-- Function to find the subtitle for a given time
findSubtitle :: [Subtitle] -> NominalDiffTime -> String
findSubtitle subs time = 
    case find (\sub -> startTime sub <= time && endTime sub >= time) subs of
        Just sub -> text sub
        Nothing  -> ""

-- Main function to read SRT file and get subtitle at given time
getSubtitleAtTime :: FilePath -> Int -> IO String
getSubtitleAtTime filePath timeInSeconds = do
    result <- try (readFile filePath) :: IO (Either IOError String)
    
    case result of
        Left _ -> return ""
        Right contents -> 
            
            let subtitles = parseSRT contents
                time = fromIntegral timeInSeconds
            in do
                -- putStrLn $ show subtitles
                return $ findSubtitle subtitles time

get_current_time :: Databus -> Int
-- get_current_time db = global_current_frame db * (global_video_length_seconds db) `div` (global_total_frames db)
get_current_time db = (global_current_frame db - 1) `div` 10

get_subtitle_from_frame :: Databus -> IO Databus
get_subtitle_from_frame db = do
    let srt_file_path = get_srt_file_path db
    current_time <- return $ get_current_time db
    subtitle <- getSubtitleAtTime srt_file_path current_time
    return $ db { global_subtitle = subtitle }
