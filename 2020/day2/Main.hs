module Main where

import Text.Parsec.String
import Text.Parsec
import Data.Foldable

{-

Author: Benjamin M.
https://github.com/cptwunderlich

How many passwords match policy?

Part1: "The password policy indicates the lowest and highest number of
    times a given letter must appear for the password to be valid."

Part2: "Each policy actually describes two positions in the password,
    exactly one of these positions must contain the given letter."

Example: 3-8 d: hddccvldwdthrc

https://adventofcode.com/2020/day/2

-}

data PassPolicy = PassP Int Int Char String

parsePassP :: GenParser Char st [PassPolicy]
parsePassP = many line

line :: GenParser Char st PassPolicy
line =
    do
        min <- read <$> many1 digit
        char '-'
        max <- read <$> many1 digit
        space
        ch <- letter
        char ':'
        space
        pw <- many1 letter
        endOfLine
        return (PassP min max ch pw)


countMatches :: (PassPolicy -> Bool) -> [PassPolicy] -> Int
countMatches matches pws
 = foldl' (\acc pw -> acc + fromEnum (matches pw)) 0 pws


part1 = countMatches matchesPolicy1


-- Count lines where either pos1 OR pos2 is equal to given char
part2 = countMatches (\(PassP p1 p2 ch pw)
    -> (pw!!(p1 - 1) == ch) /= (pw!!(p2 - 1) == ch))


-- For part1: `ch` must occur between `min` and `max` times in `pw`
matchesPolicy1 :: PassPolicy -> Bool
matchesPolicy1 (PassP min max ch pw)
 = cnt >= min && cnt <= max
 where
     cnt = foldl' (\acc c -> acc + fromEnum (c == ch)) 0 pw


main :: IO ()
main = do
    result <- parseFromFile parsePassP "input"
    case result of
                 Left err  -> print err
                 Right xs  -> print (show (part1 xs))
                                >> print (show (part2 xs))
