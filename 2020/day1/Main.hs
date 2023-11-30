{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.IntSet as IntSet
import Data.List (foldl')

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: find the two entries that sum to 2020
and then multiply those two numbers together.

Part2: what is the product of the three entries that sum to 2020?

https://adventofcode.com/2020/day/1

-}

year :: Int
year = 2020


-- Iterating over input, with current element x,
-- let y = 2020 - x and then check whether y exists in the input set.
-- That should be in O(n) (Construct set n*k, iterate n, check k, where
-- k is a constant and therefore in O(1))
part1 :: [Int] -> Int
part1 nums
 = let
     numSet = IntSet.fromList nums
     occurs n = IntSet.member n numSet
     findFactor y x = if occurs (year - x)
                        then (x, year -x)
                        else y
     summands = foldl' findFactor (-1, -1) nums
     a = fst summands
     b = snd summands

   in a * b


-- Just brute force, but surprisingly fast. Maybe Haskell magic?
-- Or my new Ryzen 4700U CPU ^^
part2 :: [Int] -> Int
part2 nums
 = let
    tmp = [x * y * z | x <- nums, y <- nums, z <- nums,
                x + y + z == year, x /= y, x /= z, y /= z]
   in head tmp


main :: IO ()
main = do
    input <- lines <$> readFile "./day1.txt"
    let (nums :: [Int]) = map read input

    putStrLn $ "Part1: " ++ show (part1 nums)
    putStrLn $ "Part2: " ++ show (part2 nums)
