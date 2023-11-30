{-# LANGUAGE OverloadedStrings #-}

module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Count unique occurences of letters [a-z] per group,
       then add all up.

Part2: Count intersection of answers in each line per group.

https://adventofcode.com/2020/day/6

-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List hiding (filter, sum, length)
import qualified Data.Set as S

prepInput :: T.Text -> [T.Text]
prepInput = T.splitOn "\n\n"

countUnique :: T.Text -> Int
countUnique t = length $ group $ sort $ T.unpack $ T.filter (/= '\n') t
 
part1 :: [T.Text] -> Int
part1 ts = sum $ map countUnique ts

part2 :: [T.Text] -> Int
part2 ts = sum $ map (S.size . intersectAll . map mkSet . T.lines) ts
 where
     mkSet          = S.fromList . T.unpack
     intersectAll   = foldl' S.intersection (S.fromList ['a'..'z'])

main :: IO ()
main = do
    TIO.interact go
 where go t = T.concat [ "\nPart 1: ", T.pack (show (part1 $ prepInput t)), "\n"
                       , "Part 2: ", T.pack (show (part2 $ prepInput t)), "\n"]
