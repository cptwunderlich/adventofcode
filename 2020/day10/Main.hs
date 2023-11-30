module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Count 1-diff and 3-diff and multiply counts.

Part2: Possible permutations to get from 0 to "outlet".

https://adventofcode.com/2020/day/10

-}

import Data.List (group,  partition, sort )

part1 :: [Int] -> Int
part1 xs    = length (fst diffs) * (1 + length (snd diffs))
 where
    diffs   = partition (< 3) $ [diff | diff <- zipWith (-) (tail xs) xs,
                diff == 1 || diff == 3]


part2 :: [Int] -> Int
part2 xs = product $ [tribonacci n | gs <- group diffs, head gs == 1, let n = length gs]
 where
    diffs = zipWith (-) (tail xs) xs


tribs :: [Int]
tribs = 1 : 1 : 2 : zipWith3 (\x y z -> x +y +z) tribs (tail tribs) (drop 2 tribs)


tribonacci :: Int -> Int
tribonacci n = tribs !! n


main :: IO ()
main = interact ((\xs -> concat ["Part 1 : ", show (part1 xs)
                              , "\nPart 2: ", show (part2 xs), "\n"])
            . (0 :) . sort . map (read :: (String -> Int)) . lines)
