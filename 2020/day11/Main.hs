module Main where

{-

Author: Benjamin M.
https://github.com/cptwunderlich

Part1: Simulate cellular automaton and find number of occupied seats
       in steady state.

Part2: Simulate it slightly differently...

https://adventofcode.com/2020/day/11

-}

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.List (find)
import Data.Maybe (mapMaybe)

data Position = Pos Int Int

data World = W (Vector (U.Vector Char)) Int Int deriving (Eq)


get :: Vector (U.Vector Char) -> (Int, Int) -> Char
get w (x, y) = (w V.! y) U.! x


neighborhood :: World -> Position -> [Char]
neighborhood (W w mx my) pos = map (get w) $ poss mx my pos


poss :: Int -> Int -> Position -> [(Int, Int)]
poss xMax yMax (Pos x1 y1) = [(x, y) | x <- [x1-1..x1+1], y <- [y1-1..y1+1], y /= y1 || x /= x1,
                            x >= 0, y >= 0, x < xMax, y < yMax]


inView :: World -> Position -> [Char]
inView (W w mx my) (Pos x y) = map (get w) $ mapMaybe dir poss'
 where
     dir = find (\p -> get w p /= '.') . takeWhile chkMv . map (mkMv (x, y))
     mkMv (x, y) (c, (sx, sy)) = (x + sx * c, y + sy * c)
     chkMv (x, y) = and [x >= 0, x < mx, y >= 0, y < my]
     poss' = [zip [1..] $ repeat (x, y) | x <- [-1..1], y <- [-1..1], y /= 0 || x /= 0]


evolve :: (World -> Position -> [Char]) -> Int -> World -> World
evolve neigh n world@(W w mx my) = W (V.imap (\y row -> U.imap
    (\x col -> step (neigh world) (busy n) (Pos x y) col) row) w) mx my


busy :: Foldable t => Int -> t a -> Bool
busy n = (n <=) . length


step :: (Position -> [Char])
     -> ([Char] -> Bool)
     -> Position -> Char -> Char

step _ _ _ '.' = '.'
step nbs busy p c
        | c == 'L' && null occupied = '#'
        | c == '#' && busy occupied = 'L'
        | otherwise                 = c
 where
     occupied = filter ('#' ==) $ nbs p


countOccupied :: World -> Int
countOccupied (W w _ _) = V.foldl'
    (\c row -> c + U.foldl' (\acc col -> acc + fromEnum (col == '#')) 0 row)
    0 w


mkWorld :: String -> World
mkWorld s = let vec = V.fromList $ map U.fromList $ lines s
            in  W vec (U.length (vec V.! 0)) (V.length vec)


part1 :: World -> Int
part1 w =
    let w' = evolve neighborhood 4 w
    in  if w == w' then countOccupied w else part1 w'


part2 :: World -> Int
part2 w =
    let w' = evolve inView 5 w
    in  if w == w' then countOccupied w else part2 w'


main :: IO ()
main = interact $ (\w -> concat ["Part 1: ", show (part1 w), "\n",
                                "Part 2: ", show (part2 w), "\n"])
                                . mkWorld
