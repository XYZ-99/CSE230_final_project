import Codec.Picture
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)

-- Define a custom character set to represent varying shades of gray
characterSet :: String
characterSet = "@%#*+=-:. "

-- Load an image file and convert it to grayscale
loadImage :: FilePath -> Int -> IO (Maybe (Image Pixel8))
loadImage filePath targetWidth = do
    imageResult <- readImage filePath
    case imageResult of
        Left _ -> return Nothing
        Right dynamicImage ->
            return . Just $ convertToAsciiResolution (convertRGB8 dynamicImage) targetWidth

-- Function to convert an RGB image to grayscale
convertRGB8ToGrayscale :: Image PixelRGB8 -> Image Pixel8
convertRGB8ToGrayscale img = pixelMap convertPixel img
  where
    convertPixel (PixelRGB8 r g b) =
      let gray = round $ (fromIntegral r * 0.3) + (fromIntegral g * 0.59) + (fromIntegral b * 0.11)
      in gray

-- Manually implemented bilinear resize function
resizeBilinear :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
resizeBilinear newWidth newHeight img = generateImage generatePixel newWidth newHeight
  where
    oldWidth = imageWidth img
    oldHeight = imageHeight img
    xRatio = fromIntegral oldWidth / fromIntegral newWidth
    yRatio = fromIntegral oldHeight / fromIntegral newHeight

    generatePixel x y = interpolate (fromIntegral x * xRatio) (fromIntegral y * yRatio)
    interpolate xf yf =
      let x = floor xf
          y = floor yf
          xWeight = xf - fromIntegral x
          yWeight = yf - fromIntegral y

          pixel00 = pixelAt img x y
          pixel01 = pixelAt img x (min (y + 1) (oldHeight - 1))
          pixel10 = pixelAt img (min (x + 1) (oldWidth - 1)) y
          pixel11 = pixelAt img (min (x + 1) (oldWidth - 1)) (min (y + 1) (oldHeight - 1))

          -- Linear interpolation in X direction
          interPixelX1 = lerpPixel xWeight pixel00 pixel10
          interPixelX2 = lerpPixel xWeight pixel01 pixel11

          -- Linear interpolation in Y direction
          finalPixel = lerpPixel yWeight interPixelX1 interPixelX2
      in finalPixel

    lerpPixel t (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2) =
      PixelRGB8 (lerp t r1 r2) (lerp t g1 g2) (lerp t b1 b2)
    lerp t a b = round $ fromIntegral a * (1 - t) + fromIntegral b * t

-- Function to convert image to a specific ASCII art resolution
convertToAsciiResolution :: Image PixelRGB8 -> Int -> Image Pixel8
convertToAsciiResolution img targetWidth =
    let originalWidth = imageWidth img
        originalHeight = imageHeight img
        aspectRatio = fromIntegral originalHeight / fromIntegral originalWidth
        targetHeight = round $ fromIntegral targetWidth * aspectRatio
    in convertRGB8ToGrayscale $ resizeBilinear targetWidth targetHeight img

-- Convert a grayscale pixel value to an appropriate character
pixelToChar :: Pixel8 -> Char
pixelToChar pixel = characterSet !! (fromIntegral pixel * (length characterSet - 1) `div` 255)

-- Convert a grayscale image to ASCII art
toAsciiArt :: Image Pixel8 -> Text
toAsciiArt image =
    T.unlines [T.pack [pixelToChar (pixelAt image x y) | x <- [0 .. imageWidth image - 1]] | y <- [0 .. imageHeight image - 1]]

main :: IO ()
main = do
    args <- getArgs
    let targetWidth = if null args then 100 else read (head args) :: Int
    maybeImage <- loadImage "example.png" targetWidth
    case maybeImage of
        Just image -> putStrLn (T.unpack (toAsciiArt image))
        Nothing    -> putStrLn "Failed to load image."
