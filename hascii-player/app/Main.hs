module Main where
import Codec.Picture
import Data.Text (Text)
import qualified Data.Text  as T
import System.Environment (getArgs)
import ASCIILoading
-- import UI
import UI
-- import Preprocess
import Subtitle
main :: IO ()
main = do
      args <- getArgs
      ui_main (args !! 0)
      -- return ()
        