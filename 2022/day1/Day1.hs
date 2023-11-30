import Data.List (sortBy)

main = interact solve

solve :: String -> String
solve input = show (part1 cals) ++ "\n" ++ show (part2 cals) ++ "\n" 
  where
    ls = lines input
    accLines acc rest = let (nonEmpty, rest') = break null rest
                            acc' = nonEmpty : acc
                        in if null rest'
                             then acc'
                             else accLines acc' $ tail rest'
    cals = map (map read) $ accLines [] ls

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortBy (flip compare) . map sum
