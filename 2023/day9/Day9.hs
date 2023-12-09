import Data.List (foldl')

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\nPart2: " ++ show p2 ++ "\n"
  where
    seqs = map (map read . words) $ lines s
    p1 = sum $ map next seqs
    p2 = sum $ map prev seqs

next :: [Int] -> Int
next s = foldl' (+) 0 $ map last $ history s

prev :: [Int] -> Int
prev s = foldl' (flip (-)) 0 $ map head $ history s

history :: [Int] -> [[Int]]
history l = reverse $ takeWhile (any (0 /=)) $ iterate diffSeq l
  where
    diffSeq s = zipWith (-) (drop 1 s) s
