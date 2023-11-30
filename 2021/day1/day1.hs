module Main where

import Data.List

window' :: Int -> [a] -> [[a]]
window' n xs = [w | ts <- tails xs, let w = take n ts, length w == n]

countIncs xs = sum $ map (fromEnum . uncurry (<)) $ zip xs $ tail xs

parse :: String -> [Int]
parse = map read . lines

main :: IO ()
main = interact (perform . parse)
 where
    part2 = map sum . window' 3
    perform xs = concat ["Part 1: ", show $ countIncs xs, "\n",
        "Part 2: ", show $ countIncs $ part2 xs, "\n"]

