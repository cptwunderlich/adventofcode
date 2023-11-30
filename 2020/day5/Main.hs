{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Foldable (Foldable(foldl'))
import Data.List (sort)

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part 1: Find highest seat ID.
        Starting from Interval 0-127, perform 7 halvings and choose the
        interval part indicated by input ('F' lower part, 'B' upper).
        For the last 3 characters, handle interval 0-7.
        seat ID = row * 8 + col

Part 2: Given list of seat IDs, find missing ID, where ID-1 and ID+1 exist.

https://adventofcode.com/2020/day/5

-}

chrToBit :: Char -> Int
chrToBit = \case
        'F' -> 0
        'L' -> 0
        'B' -> 1
        'R' -> 1


bstrToDec :: [Int] -> Int
bstrToDec = foldr1 (\b d -> b + 2 * d) . reverse


seatIds :: [String] -> [Int]
seatIds = map (bstrToDec . map chrToBit)


part2 :: [Int] -> Int
part2 sids = findMissing $ zip sorted $ tail sorted
 where
         sorted         = sort sids
         findMissing p  = foldl' (\m (x, y) -> if y - x == 2 then x+1 else m) 0 p


main :: IO ()
main = interact go
 where
         sids    = seatIds . lines
         go inp  = "Part 1: " ++ show (maximum $ sids inp)
                ++ "\nPart 2: " ++ show (part2 $ sids inp) ++ "\n"