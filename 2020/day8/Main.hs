module Main where

import qualified Data.IntSet as S
import Data.Maybe (fromMaybe)

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Find value of accumulator before loop in program.

Part2: Fix corrupted instruction and return accumulator at program termination.

https://adventofcode.com/2020/day/8

-}

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int
                 deriving (Eq, Show)

parseInput :: String -> [Instruction]
parseInput = map ((\x -> toInstr (head x) (toNum $ last x)) . words) . lines
 where
     toNum = read . dropWhile (== '+')
     toInstr i n = case i of
         "nop"      -> Nop n
         "acc"      -> Acc n
         "jmp"      -> Jmp n
         _          -> error $ "Unknown opcode: " ++ i

part1 :: [Instruction] -> Int
part1 is = 
 let
     len = length is     
     exec acc ip seen ins
        | fst i `S.member` seen     = acc
        | otherwise                 = exec acc' ip' (S.insert (fst i) seen) ins
      where
            i           = ins !! ip
            (acc', ip') = case snd i of
                Nop _   -> (acc,     succ ip `mod` len)
                Acc n   -> (acc + n, succ ip `mod` len)
                Jmp n   -> (acc,     (ip + n) `mod` len)
 in exec 0 0 S.empty $ zip [0..] is

exec :: [Instruction] -> Maybe Int
exec is = 
 let
     len = length is     
     exec' acc ip seen ins
        | ip == len                 = Just acc
        | (ip < 0 || ip > len)
            || fst i `S.member` seen
                                    = Nothing
        | otherwise                 = exec' acc' ip' (S.insert (fst i) seen) ins
      where
            i           = ins !! (ip `mod` len)
            (acc', ip') = case snd i of
                Nop _   -> (acc,     succ ip)
                Acc n   -> (acc + n, succ ip)
                Jmp n   -> (acc,     ip + n)
 in exec' 0 0 S.empty $ zip [0..] is

part2 :: [Instruction] -> Int
part2 = go []
 where
     swap i = case i of
         Nop n  -> Jmp n
         Jmp n  -> Nop n
         _      -> i

     go _ []            = error "Empty list!"
     go begin (i:iss)   = fromMaybe (go (begin ++ [i]) iss) (exec (begin ++ (swap i : iss)))


main :: IO ()
main = do
    interact (go . parseInput)
 where
     go ins = concat [ "Part 1: ", show (part1 ins), "\n"
                     , "Part 2: ", show (part2 ins), "\n" ]
