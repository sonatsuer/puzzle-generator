{-# LANGUAGE NoMonomorphismRestriction #-}
module Puzzle (makeNoisyPuzzle) where

import Data.List (transpose, unfoldr)
import Control.Monad (liftM2)
import System.Random
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

type Vec = V2 Double

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

diagonal3 :: (a -> b -> c) -> ((a, a, a) -> (b, b, b) -> (c, c, c))
diagonal3 f (x1, x2, x3) (y1, y2, y3) = (f x1 y1, f x2 y2, f x3 y3)

altBool :: Bool -> [Bool]
altBool b = cycle [b, not b]

altSeq :: Bool -> [(Double, Double)] -> Diagram B
altSeq b = fromSegments . concat . zipWith piece (altBool b)

altSeqs :: Bool -> Double -> [[(Double, Double)]] -> Diagram B
altSeqs b step xss = position $ zip pos seqs 
  where seqs = zipWith ($) fs xss
        fs   = map altSeq (altBool b)
        pos  = [ p2 (0, i) | i <- [0, step .. ]]

makePuzzle :: [[(Double, Double)]] -> [[(Double, Double)]] -> Diagram B
makePuzzle rows cols = (inside <> boundingRect inside) # lwG 0.1
  where inside  = hors <> vers
        vers    = altSeqs True 3 cols # rotateBy (1/4)
        hors    = altSeqs True 3 rows # translate v
        v       = V2 x 3 :: V2 Double
        x       = -3 * (fromIntegral $ length cols)

makeNoisyPuzzle :: [[Double]] -> [[Double]] -> IO(Diagram B)
makeNoisyPuzzle xss yss
  = do xss' <- addNoise xss
       yss' <- addNoise yss
       return $ makePuzzle xss' yss'
                         
groupBy :: Int -> Int -> [a] -> [[a]]
groupBy w h xs = take h $ unfoldr aux xs
  where aux l = Just (take w l, drop w l)
  
randomTable :: Double -> Int -> Int -> IO ([[Double]])
randomTable r h w = fmap (groupBy w h) rnd
  where rnd = fmap (randomRs (-r, r)) newStdGen

noiseConstant :: Double
noiseConstant = 0.55

addNoise :: [[Double]] -> IO([[(Double, Double)]])
addNoise xss = let (w, h) = (length $ head xss, length xss) in
  do tbl <- randomTable noiseConstant h w
     return $ (zipWith zip) tbl xss

bzTuple :: (Vec, Vec, Vec) -> Segment Closed V2 Double
bzTuple = uncurry3 bezier3'
  where bezier3' c1 c2 x2 = bezier3 c1 (x2 ^-^ c2) x2
        
smallP :: [(Vec, Vec, Vec)]
smallP = [(0.1 ^& 0,   0.1 ^& 0.1, 0.2 ^& 0.05),
          (0.1 ^& 0.1, 0.1 ^& (-0.1),  2.6 ^& 0),
          (0.1 ^& (-0.1), 0.1 ^& 0, 0.2 ^& (-0.05) )]

largeP :: [(Vec, Vec, Vec)]
largeP = [(0.3     ^& 0,       (-0.75) ^& 0.85,    1 ^& 0.5),
          ((-0.75) ^& 0.85 ,   (-0.75) ^& (-0.85), 1 ^& 0),
          ((-0.75) ^& (-0.85), 0.3     ^& 0,       1 ^& (-0.5))]

noiseP :: Double -> [(Vec, Vec, Vec)]
noiseP s = [(0 ^& 0, 0 ^& s,    0 ^& 0),
            (0 ^& 0, 0 ^& (-s), 0 ^& 0),
            (0 ^& 0, 0 ^& 0,    0 ^& 0)]

piece :: Bool -> (Double, Double) -> [Segment Closed V2 Double]
piece reflected (s,t) =
  if reflected then map reflectY segs else segs
  where segs = map bzTuple final
        final      = zipWith (diagonal3 (lerp t)) smallNoise largeNoise
        smallNoise = zipWith (diagonal3 (^+^)) smallP (noiseP $ s / 3)
        largeNoise = zipWith (diagonal3 (^+^)) largeP (noiseP s)
        
