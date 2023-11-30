{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec.String
import Text.Parsec
import Data.Char
import qualified Data.Map as Map
import Text.Read (readMaybe)

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part 1: 

required fields:
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)

Optional:
    cid (Country ID) 


Part 2: 

    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.

(text source: adventofcode.com, copyright Eric Wastl)

https://adventofcode.com/2020/day/4

-}

type PpMap = Map.Map String String


parsePassports :: Parsec String st [PpMap]
parsePassports = concat <$> many1 (sepEndBy1 passport newline)


checkValid :: PpMap -> Int
checkValid s = fromEnum $ and [ has "byr" s, has "iyr" s, has "eyr" s
                              , has "hgt" s, has "hcl" s, has "ecl" s
                              , has "pid" s ]
 where has x = Map.member x


checkValid2 :: PpMap -> Int
checkValid2 s = fromEnum $ and [ has byr "byr" s, has iyr "iyr" s
                               , has eyr "eyr" s, has hgt "hgt" s
                               , has hcl "hcl" s, has ecl "ecl" s
                               , has pid "pid" s ]
 where
        has f x m = maybe False f $ Map.lookup x m

        validDate from to v
         = case readMaybe v :: Maybe Int of
                Just y  -> from <= y && y <= to
                Nothing -> False

        validParse p v = case parse p "" v of
            Left _  -> False
            Right _ -> True

        byr = validDate 1920 2002
        iyr = validDate 2010 2020
        eyr = validDate 2020 2030
        hgt v = case parse height "" v of
            Left _          -> False
            Right (h, u)    -> if u == "cm"
                                then h >= 150 && h <= 193
                                else h >= 59 && h <= 76
        hcl = validParse hairColor
        ecl v = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        pid = validParse passportId


height :: Parsec String st (Int, String)
height = do
    num <- many1 digit
    unit <- string "cm" <|> string "in"
    eof
    return (read num, unit)


hairColor :: Parsec String st String
hairColor = char '#' >> count 6 (digit <|> oneOf ['a'..'f']) <* eof


passportId :: Parsec String st String
passportId = count 9 digit <* eof


passport :: Parsec String st PpMap
passport = Map.fromList <$> sepEndBy1 datum space


datum :: Parsec String st (String, String)
datum = do
    ident <- count 3 letter
    char ':'
    val <- many1 $ satisfy (not . isSpace)

    return (ident, val)


part1 :: [PpMap] -> Int
part1 = sum . map checkValid


part2 :: [PpMap] -> Int
part2 = sum . map checkValid2


main :: IO ()
main = do
    result <- parseFromFile parsePassports "input"
    case result of
            Left err  -> print err
            Right res -> print ("Part 1: " ++ show (part1 res))
                            >> print ("Part 2: " ++ show (part2 res))
