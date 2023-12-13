{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Preprocess where
-- module Main where

import Data.Bits
import Data.Foldable
import System.Process

-- global
preprocess_file_extension = ".jpeg"

hash :: String -> Int
hash = foldl' (\h c -> 33 * h `xor` fromEnum c) 5381

-- >>> preprocess_filepath_hash "hoge"
-- "6382760864"

preprocess_filepath_hash :: String -> String
preprocess_filepath_hash filepath = show $ hash filepath
preprocess_filepath_hash filepath = show $ hash filepath

preprocess_make_dir_if_not_exists :: String -> IO ()
preprocess_make_dir_if_not_exists dir = callCommand $ "mkdir -p " ++ dir

preprocess_convert_command :: String -> String -> String
preprocess_convert_command source target_dir = "ffmpeg -i " ++ source ++ " -r 10 " ++ target_dir ++ "/frames/$filename%08d" ++ preprocess_file_extension

make_dir_if_not_exists :: String -> IO ()
make_dir_if_not_exists dir = callCommand $ "mkdir -p " ++ dir

preprocess_video_to_frame :: String -> String -> IO ()
preprocess_video_to_frame source_video_path target_dir = do
  let hashed = preprocess_filepath_hash source_video_path
  make_dir_if_not_exists target_dir
  make_dir_if_not_exists $ target_dir ++ "/" ++ hashed
  make_dir_if_not_exists $ target_dir ++ "/" ++ hashed ++ "/frames"
  callCommand $ preprocess_convert_command source_video_path $ target_dir ++ "/" ++ hashed


