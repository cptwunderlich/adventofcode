import Data.Char (isDigit)
import Data.List (foldl', intersect, unfoldr)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type CardId = Int
type Matches = Int

data Card = Card {cid :: CardId, matches :: Matches, cnt :: Int} deriving (Show)

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\nPart2: " ++ show p2 ++ "\n"
  where
    summary = M.fromList $ map ((\c -> (cid c, c)) . parseLine) (lines s)
    p1 = part1 summary
    p2 = part2 summary

part1 :: Map CardId Card -> Int
part1 = M.foldl' (\acc c -> acc + if matches c == 0 then 0 else 2 ^ (matches c - 1)) 0

part2 :: Map CardId Card -> Int
part2 cards = M.foldl' (\acc c -> acc + cnt c) 0 (dupAll cards)
  where
    dupAll sm = M.foldl' duplicateCards sm sm

duplicateCards :: Map CardId Card -> Card -> Map CardId Card
duplicateCards sm c = foldl' update sm $ mkDupIds c
  where
    (Card _ _ cnt) = sm M.! cid c
    mkDupIds (Card cid matches _) = unfoldr (genNextId (cid + matches)) cid
    genNextId max x = if x < max then Just (succ x, succ x) else Nothing
    update m k = M.adjust (adjustCnt cnt) k m

findMatches :: [Int] -> [Int] -> [Int]
findMatches winning have = winning `intersect` have

parseLine :: String -> Card
parseLine s = mkCard cid $ length $ findMatches winning have
  where
    cid = read $ takeWhile isDigit $ dropWhile (not . isDigit) s
    rhs = drop 2 $ dropWhile (':' /=) s
    (winning, have) = case break ('|' ==) rhs of
        (ls, rs) -> mapPair (map read . words) (ls, drop 1 rs)

mapPair :: (t -> b) -> (t, t) -> (b, b)
mapPair f (x, y) = (f x, f y)

mkCard :: CardId -> Matches -> Card
mkCard c m = Card c m 1

adjustCnt :: Int -> Card -> Card
adjustCnt x (Card cid ms cnt) = Card cid ms $ cnt + x
