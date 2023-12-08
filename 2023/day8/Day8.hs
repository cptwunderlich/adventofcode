{-# LANGUAGE BangPatterns #-}

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

data Branch = Branch String String deriving Show

type Moves = [Char]
type Graph = Map String Branch

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\nPart2: " ++ show p2 ++ "\n"
  where
    (moves, graph) = parse s
    p1 = walk "AAA" ("ZZZ" ==) moves graph
    p2 = part2 moves graph

part2 :: Moves -> Graph -> Int
part2 moves graph = lcmAll cycleSteps
    where
        starts = filter (\s -> drop 2 s == "A") $ M.keys graph
        isEnd s = drop 2 s == "Z"
        cycleSteps = map (\s -> walk s isEnd moves graph) starts

lcmAll :: [Int] -> Int
lcmAll = foldr1 lcm

walk :: String -> (String -> Bool) -> Moves -> Graph -> Int
walk start isEnd ms g = go 0 start (cycle ms) g
    where
        go !steps pos (m : ms) g
          | isEnd pos = steps
          | otherwise =
            let (Branch l r) = g M.! pos
                next = if m == 'R' then r else l
            in go (succ steps) next ms g

parse :: String -> (Moves, Graph)
parse s = (moves, M.fromList $ map mkEntry nodes)
    where
        ls = lines s
        moves = head ls
        nodes = drop 2 ls
        mkEntry x = let ws = words x in (head ws, Branch ( take 3 $ drop 1 $ ws !! 2) (take 3 $ ws !! 3))
