module Subtitle where


import System.IO
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import Data.List (find)
import Control.Exception (catch, IOException)


import Databus
import Preprocess(hash)


-- Define a data structure for a subtitle block
data Subtitle = Subtitle {
    startTime :: UTCTime,
    endTime   :: UTCTime,
    text      :: String
} deriving (Show)

splitOn :: String -> String -> [String]
splitOn delimiter = foldr f [[]]
    where f c l@(x:xs) | take (length delimiter) (c:x) == delimiter = []:xs
                       | otherwise = (c:x):xs

-- Function to parse a time string from the SRT file
parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%H:%M:%S,%q"

-- Function to parse a single subtitle block
parseBlock :: [String] -> Subtitle
parseBlock (timeStr:textLines) =
    let [startStr, endStr] = words $ map (\c -> if c == ',' then '.' else c) $ head $ lines timeStr
        start = parseTime startStr
        end = parseTime endStr
        text = unlines textLines
    in Subtitle start end text

-- Function to split the input into blocks
splitBlocks :: String -> [Subtitle]
splitBlocks input =
    map parseBlock $ filter (not . null) $ map lines $ splitOn "\n\n" input


-- Function to find the current subtitle
findCurrentSubtitle :: Int -> [Subtitle] -> Maybe String
findCurrentSubtitle currentTime subs =
    let currentUTCTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime $ fromIntegral currentTime)
    in text <$> find (\sub -> startTime sub <= currentUTCTime && endTime sub >= currentUTCTime) subs


maybe_to_string :: Maybe String -> String
maybe_to_string (Just a) = a
maybe_to_string Nothing = ""



get_srt_file_path :: Databus -> String
get_srt_file_path db = global_subtitle_path db



parseSRT :: FilePath -> Int -> IO String
parseSRT filePath currentTime = catch readAndProcessFile handleException
  where
    readAndProcessFile :: IO String
    readAndProcessFile = do
        fileContent <- readFile filePath
        let subtitles = splitBlocks fileContent
        return $ maybe_to_string $ findCurrentSubtitle currentTime subtitles

    handleException :: IOException -> IO String
    handleException _ = return ""



get_current_time :: Databus -> Int
-- get_current_time db = global_current_frame db * (global_video_length_seconds db) `div` (global_total_frames db)
get_current_time db = (global_current_frame db - 1) * 10 

get_subtitle_from_frame :: Databus -> IO Databus
get_subtitle_from_frame db = do
    let srt_file_path = get_srt_file_path db
    current_time <- return $ get_current_time db
    subtitle <- parseSRT srt_file_path current_time
    return $ db { global_subtitle = subtitle }
