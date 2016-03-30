module Table (imageToRowsAndColums) where

import Codec.Picture
import Control.Monad (liftM)
import Data.List (unfoldr, transpose)

brightness :: PixelRGB8 -> Double
brightness (PixelRGB8 r g b)
  = (fromIntegral r + fromIntegral g + fromIntegral b) / 768

loadAsRGB8 :: String -> IO (Image PixelRGB8)
loadAsRGB8 = liftM f . readImage
  where f (Left err)  = error err
        f (Right img) = convertRGB8 img

check :: Int -> Int -> Bool
check x y = x `mod` 4 == 0 && y `mod` 4 == 0

msg :: String
msg =
  "Both the width and the height of the input image should be divisible by four."
  
imageToTable :: Image PixelRGB8 -> [[Double]]
imageToTable img =
  if check w h
     then [ row y | y <- [h - 1, h - 2 .. 0]]
          else error msg       
  where w     = imageWidth img
        h     = imageHeight img 
        row i = [brightness $ pixelAt img x i  | x <- [0 .. w-1]]

mergeAlt :: Bool -> [Double] -> [Double] -> [Double]
mergeAlt b l1 l2 = case (b, l1, l2) of

  (True, x1 : x2 : xs, _ : _ : ys) ->
    (x1 + x2) / 2.0 : mergeAlt False xs ys

  (False, _ : _ : xs, y1 : y2 : ys) ->
    (y1 + y2) / 2.0 : mergeAlt True xs ys

  (_, _, _) ->
    []

prepareRows :: Bool -> [[Double]] -> [[Double]]
prepareRows b = aux b . drop 1 where
  aux bl rs = case (bl, rs) of
    (b, r1 : r2 : rest) ->
      mergeAlt b r1 r2 : aux (not b) rest
    (_, _) ->
      []

 
rowsAndColums :: [[Double]] -> ([[Double]], [[Double]])
rowsAndColums table = (rows, colums)
  where rows   = prepareRows True table
        colums = prepareRows True (transpose $ map reverse table)

imageToRowsAndColums :: String -> IO([[Double]], [[Double]])
imageToRowsAndColums path = 
  liftM (rowsAndColums . imageToTable ) (loadAsRGB8 path)
     
