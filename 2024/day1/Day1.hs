import Data.List (group, sort, transpose)
import qualified Data.Map.Strict as M

toSortedCols :: String -> [[Int]]
toSortedCols = map sort . transpose . map (map (read @Int) .  words) . lines

part1 :: [[Int]] -> Int
part1 = sum . uncurry (zipWith (\l r -> abs $ l - r)) . toTuple
    where toTuple [xs, ys] = (xs, ys)

part2 :: [[Int]] -> Int
part2 [ls, rs] = sum $ map (\l -> l * M.findWithDefault 0 l lut) ls
    where lut = M.fromList $ map (\xs -> (head xs, length xs)) $ group rs

main = interact solve
    where
        solve = (\cs -> unlines [show $ part1 cs, show $ part2 cs]) . toSortedCols
