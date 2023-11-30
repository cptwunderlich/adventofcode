module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Find the factor with the smallest remainder vs given value
       and return factor * remainder.

Part2: Find the number from which all given values have a multiple such
       that each following is shifte by its position.

(code for part 2 adapted from https://rosettacode.org/wiki/Chinese_remainder_theorem)

https://adventofcode.com/2020/day/13

-}

import Aoc (interact', splitOn)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Bifunctor (Bifunctor(second))


part1 :: (Int, [Int]) -> Int
part1 (t, ids) =
 let res = minimumBy (compare `on` snd) $ map (\n -> (n, n - (t `mod` n))) ids
 in  uncurry (*) res


part2 :: [(Int, Int)] -> Int
part2 xs = uncurry chineseRemainder $ unzip xs


egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b


modInv :: Int -> Int -> Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> x
      | otherwise ->
        error $ "No modular inverse for " ++ show a ++ " and " ++ show b


chineseRemainder :: [Int] -> [Int] -> Int
chineseRemainder residues mods =
  ((`mod` modPI) . sum . zipWith (*) crtMods . zipWith (*) residues) $ zipWith modInv crtMods mods
  where
    modPI     = product mods
    crtMods   = (modPI `div`) <$> mods


main :: IO ()
main = interact' (parse . lines) part1' part2'
 where
     parse ls   = (read $ head ls, map (second read) $ filter (("x" /=) . snd) $ zip [0..] $ splitOn (',' ==) $ last ls)
     part1'     = part1 . second (map snd)
     part2'     = part2 . map (\(x, y) -> (y - x, y)) . snd