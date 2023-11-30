module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Find numbers that sum up to current one in last 25.

Part2: Find contiguous subset that adds up to solution to part1.

https://adventofcode.com/2020/day/9

-}

import Data.List ( inits, tails )

part1 :: [Int] -> Int
part1 xs = go (take 25 xs) $ drop 25 xs
 where
     findSum zs z = [(x, y) | x <- zs, y <- zs, x /= y, x + y == z]
     go pre (x:xs) = if null $ findSum pre x
                        then x
                        else go (tail pre ++ [x]) xs

part2 :: Int -> [Int] -> Int
part2 s xs =
 let solset = head $ filter (\subs -> length subs >= 2 && sum subs == s) $ concatMap inits $ tails xs
 in minimum solset + maximum solset

main :: IO ()
main = interact go
 where
     p1 xs = (xs, part1 xs)
     go = (\x -> concat [ "Part 1: ", show (snd x)
                        , "\nPart 2: ", show (part2 (snd x) (fst x)), "\n"])
        . p1 . map read . lines
