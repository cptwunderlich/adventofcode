module Aoc (
    Position(),
    interact',
    vadd,
    vdiff,
    vmul,
    countBy,
    window,
    window',
    splitOn
) where

import Data.List (inits, tails, foldl')

{-

Author: Benjamin M.
https://github.com/cptwunderlich/adventofcode2020

Utility functions for Advent of Code

-}


data Position = Pos Int Int deriving (Show)

interact' :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> IO ()
interact' parse p1 p2 = interact perform
 where
     perform inp = let inp' = parse inp
                   in  concat ["Part 1: ", show (p1 inp'), "\n",
                               "Part 2: ", show (p2 inp'), "\n"]

vadd :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
vadd (u, v) (x, y) = (u + x, v + y)

vdiff :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
vdiff (u, v) (x, y) = (u - x, v - y)

vmul :: (Num a) => a -> (a, a) -> (a, a)
vmul s (x, y) = (x * s, y * s)

-- | Count elements in t matching predicate.
countBy :: (Foldable t) => (a -> Bool) -> t a -> Int
countBy p = foldl' (\acc x -> if p x then succ acc else acc) 0

-- | Create sliding windows of size n over a list, including shorter boundary windows.
window :: Int -> [a] -> [[a]]
window n xs
   | null xs || n <= 0  = []
   | otherwise          = drop 1 $ take n (inits xs) ++ map (take n) (init $ tails xs)

-- | Create slinding windows over list, but cut away boundary windows smaller than n.
window' :: Int -> [a] -> [[a]]
window' n xs = [w | ts <- tails xs, let w = take n ts, length w == n]

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
                    "" -> []
                    s' -> w : splitOn p s''
                         where (w, s'') = break p s'