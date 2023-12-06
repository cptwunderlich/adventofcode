import Data.Char (isDigit)
import Data.List (find)

type Time = Int
type Dist = Int

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\nPart2: " ++ show p2 ++ "\n"
  where
    records = parse s
    p1 = part1 records
    p2 = uncurry numWinningWays $ parse2 s

part1 :: [(Time, Dist)] -> Int
part1 xs = product $ map (uncurry numWinningWays) xs

numWinningWays :: Time -> Dist -> Int
numWinningWays t d =
    length $
        filter (> d) $
            zipWith (*) {- charge -} [0 .. t {- race -}] $
                reverse [0 .. t]

parse :: String -> [(Time, Dist)]
parse s =
    let (ts : ds : rest) = lines s
        parseNums = map read . words . dropWhile (not . isDigit)
        times = parseNums ts
        dists = parseNums ds
     in zip times dists

parse2 :: String -> (Time, Dist)
parse2 s =
    let (ts : ds : rest) = lines s
        parseNum = read . concat . words . dropWhile (not . isDigit)
     in (parseNum ts, parseNum ds)
