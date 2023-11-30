{-# LANGUAGE ScopedTypeVariables #-}
module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Count number of bag types that may contain a "shiny gold" bag.

Part2: Count total number of bags nested in "shiny golden" bag.

https://adventofcode.com/2020/day/7

-}

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec.String
import Text.Parsec

type Bags = [(String, [(Int, String)])]

gold = "shiny gold"

parseBags :: GenParser Char st Bags
parseBags = endBy line (char '\n') <* spaces <* eof
 where
     color = unwords <$> count 2 (many1 (noneOf " ") <* space)
     bag = color <* string "bag" <* optional (char 's')
     numberedBag = (,) <$> (read <$> many1 digit) <*> (space *> bag)
     rhs = ([] <$ string "no other bags") <|> sepBy1 numberedBag (string ", ")
     line = (,) <$> (bag <* string " contain ") <*> rhs <* char '.'

part1 :: Bags -> Int
part1 bags = pred $ S.size $ expand $ S.singleton gold
 where
     bags' = map (\(x, y) -> (x, S.fromList $ map snd y)) bags
     expand set =
      let set' = S.union set $ S.fromList $ map fst $ filter (not . S.disjoint set . snd) bags'
      in if set == set'
          then set
          else expand set'

part2 :: Bags -> Int
part2 bags = lookupAll gold
 where
     bags' = M.fromList bags
     lookupAll clr = sum
        $ maybe [] (map (\(c, b) -> c * (1 + lookupAll b))) 
        $ M.lookup clr bags'

main :: IO ()
main = do
    res <- parseFromFile parseBags "input"
    case res of
        Left err -> print err
        Right bags -> print ("Part 1: " ++ show (part1 bags))
                            >> print ("Part 2: " ++ show (part2 bags))
