module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1:

Part2:
https://adventofcode.com/2020/day/14

-}

import Aoc (interact', splitOn)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Bifunctor (Bifunctor(second))


part1 :: String -> Int
part1 = undefined


part2 :: String -> Int
part2 = undefined


main :: IO ()
main = interact' (parse . lines) part1' part2'
 where
     parse ls   = (read $ head ls, map (second read) $ filter (("x" /=) . snd) $ zip [0..] $ splitOn (',' ==) $ last ls)
     part1'     = part1 . second (map snd)
     part2'     = part2 . map (\(x, y) -> (y - x, y)) . snd