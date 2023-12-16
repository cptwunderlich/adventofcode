import Data.Char (digitToInt, isDigit)
import Data.List (group, sort, sortBy)

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\nPart2: " ++ show p2 ++ "\n"
  where
    hands = parse s
    p1 = part1 hands
    p2 = 0

part1 :: [(String, Int)] -> Int
part1 hands = sum $ zipWith (\n x -> n * snd x) [1 ..] sorted
  where
    sorted = sortBy (\(l, _) (r, _) -> l `compareHands` r) hands

parse :: String -> [(String, Int)]
parse s = map (\x -> let ws = words x in (head ws, read (ws !! 1))) $ lines s

handType :: String -> Int
handType s = let gd = group $ sort s in maximum (map length gd) - length gd

compareHands :: String -> String -> Ordering
compareHands l r = let res = l `cmpType` r in if res == EQ then l `cmpSameType` r else res
  where
    cmpType l r = handType l `compare` handType r
    cmpSameType l r = cardVals l `compare` cardVals r
    cardVals = map value

value :: Char -> Int
value c
    | isDigit c = digitToInt c
    | otherwise = case c of
        'T' -> 10
        'J' -> 11
        'Q' -> 12
        'K' -> 13
        'A' -> 14
