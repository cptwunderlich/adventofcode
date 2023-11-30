module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Follow instructions and calculate manhattan distance.

Part2: Instructions pertain waypoint, ship follows waypoint.

https://adventofcode.com/2020/day/12

-}

import Aoc (interact', vadd, vmul)

deg2rad :: Integral a => a -> Double
deg2rad d   = (fromIntegral d * pi) / 180


rotate :: (Integral a) => a -> a -> a -> (a, a)
rotate d x y =
 let b = deg2rad d
     x' = fromIntegral x
     y' = fromIntegral y
 in (round $ cos b * x' + sin b * y',
     round $ -sin b * x' + cos b * y')


normv :: (Integral a) => (a, a) -> (a, a)
normv (x, y) = let l = sqrt $ fromIntegral (x^2 + y^2) :: Double
               in  (round $ fromIntegral x / l, round $ fromIntegral y / l)


navigate :: [(Char, Int)] -> Int
navigate mvs = let (x, y) = go 90 (0, 0) mvs
               in abs x + abs y
 where
     go _ p []                  = p
     go o p@(x, y) ((i, num):ms) = case i of
            'N' -> go o (x, y + num) ms
            'S' -> go o (x, y - num) ms
            'E' -> go o (x + num, y) ms
            'W' -> go o (x - num, y) ms
            'L' -> go ((o - num) `mod` 360) p ms
            'R' -> go ((o + num) `mod` 360) p ms
            'F' -> go o (vadd p (uncurry (rotate o) (0, num))) ms
            _   -> error $ "Unknown instruction: " ++ [i]


navigate2 :: [(Char, Int)] -> Int
navigate2 mvs = let (x, y) = go (10, 1) (0, 0) mvs
               in abs x + abs y
 where
     go _ p []                     = p
     go w@(w1, w2) p ((i, num):ms) = case i of
            'N' -> go (w1, w2 + num) p ms
            'S' -> go (w1, w2 - num) p ms
            'E' -> go (w1 + num, w2) p ms
            'W' -> go (w1 - num, w2) p ms
            'L' -> go ((uncurry $ rotate ((-num) `mod` 360)) w) p ms
            'R' -> go ((uncurry $ rotate (num `mod` 360)) w) p ms
            'F' -> go w (vadd p $ vmul num w) ms
            _   -> error $ "Unknown instruction: " ++ [i]


main :: IO ()
main = interact' (map (\l -> (head l, read $ tail l)) . lines) navigate navigate2