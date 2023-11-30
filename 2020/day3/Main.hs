module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part 1: Go through 2D array 1 down, 3 right - wrapping around -
    and count '#' until "end".

Part 2: Like 1, but with variable steps, multiple step arrangements
    and multiply all results.

    Right 1, down 1.
    Right 3, down 1.
    Right 5, down 1.
    Right 7, down 1.
    Right 1, down 2.


https://adventofcode.com/2020/day/3

-}

import Data.Foldable


countTrees :: [String] -> Int -> Int -> Int
countTrees xs stepDown stepRight = foldl' checkTree 0 $ zip [i | i <- [0..], i `mod` stepRight == 0] fxs
 where checkTree cnt (s, ys) = cnt + fromEnum ((cycle ys !! s) == '#')
       fxs = map snd $ filter (\x -> fst x `mod` stepDown == 0) $ zip [0..] xs


part1 :: [String] -> Int
part1 xs = countTrees xs 1 3


part2 :: [String] -> [(Int, Int)] -> Int
part2 xs steps = product $ map ct steps
 where ct = uncurry $ countTrees xs


main :: IO ()
main = do
    input <- lines <$> readFile "./input"

    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)])