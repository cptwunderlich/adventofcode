import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (elemIndex, intersect)
import Data.Maybe (maybe)

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x:xs) = map ((,) x) xs ++ combinations xs

hamming :: [Char] -> [Char] -> Int
hamming as bs = sum $ map fromEnum $ zipWith (/=) as bs

common :: [Char] -> [Char] -> [Char]
common xs ys = [a | (a, b) <- zip xs ys, a == b]

frequencyCount :: [Char] -> Map Char Int
frequencyCount cs = 
    let inc = \key new old -> old + 1 :: Int
        updateFreq = \c cnts -> Map.insertWithKey inc c 1 cnts
    in foldr updateFreq Map.empty cs

repCount :: String -> (Int, Int)
repCount id = (fromEnum $ elem 2 cnts, fromEnum $ elem 3 cnts)
                where cnts = Map.elems $ frequencyCount id 

main = do
    input <- readFile "./input.txt"
    let ids = lines input
    let accumCnt = foldr (\(l2, l3) (r2, r3) -> (l2 + r2, l3 + r3)) (0, 0) (map repCount ids)
    let prod = fst accumCnt * snd accumCnt
    print ("Part1: Checksum: " ++ (show prod))
    let distances = map (uncurry hamming) (combinations ids)
    let i = elemIndex 1 distances
    let zipped = zip distances (combinations ids)
    print ("Part2: " ++ ((uncurry common) $ snd (maybe (-1, ("", "")) ((!!) zipped) i)))
