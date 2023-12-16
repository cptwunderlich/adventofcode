import Data.List (find, foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

type Coord = (Int, Int)
data Dir = North | East | South | West deriving (Show, Eq)
data State = Open | Close deriving (Show)

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\nPart2: " ++ show p2 ++ "\n"
  where
    tileCoords = parse s
    startPos = fst (fromJust (find (('S' ==) . snd) tileCoords))
    tileMap = M.fromList tileCoords
    start = head $ startPoss tileMap startPos
    p1 = part1 tileMap start
    p2 = part2 s tileMap startPos start

part1 :: Map Coord Char -> (Coord, Dir) -> Int
part1 m start =  (1 + length (iterWalk m start)) `div` 2

iterWalk m s =
    let tmp = uncurry (walk m) s
     in case tmp of
            Nothing -> []
            Just x -> s : iterWalk m x

part2 str m s0 (startPos, dir) = S.size insides
  where
    loop = S.fromList $ s0 : map fst (iterWalk m (startPos, dir))
    insides = traceShowId $ S.unions $ zipWith (flip $ mark loop) [0..] (lines str)

-- TODO: only count some elements? No J??
mark loop s y = (\(_, _, res) -> res) $ foldl' f (Close, 0, S.empty) $ zip [0..] s
  where
    f (st, cnt, s) (x, '.') = (st, cnt,) $ if odd cnt then S.insert (x, y) s else s
    f (st, cnt, s) (x, c) = if S.member (x, y) loop
                            then case c of
                                  _ | c `elem` "LSF" -> (Open, cnt, s)
                                  _ | c `elem` "7J" -> (Close, succ cnt, s)
                                  _        -> (st, cnt, s)
                            else (st, cnt, s)

walk m pos dir = if tile == 'S' then Nothing else Just (next, dir')
  where
    tile = m M.! pos
    dir' = nextDir dir tile
    next = nextPos pos dir'

startPoss m p = catMaybes [north, south, east, west]
  where
    valid opts p = M.lookup p m >>= \c -> if c `elem` opts then Just p else Nothing
    north = (,North) <$> valid ['|', 'F', '7'] (nextPos p North)
    south = (,South) <$> valid ['|', 'L', 'J'] (nextPos p South)
    east = (,East) <$> valid ['-', 'J', '7'] (nextPos p East)
    west = (,West) <$> valid ['-', 'F', 'L'] (nextPos p West)

parse :: String -> [((Int, Int), Char)]
parse s = [((x, y), c) | (y, l) <- zip [0 ..] $ map (zip [0 ..]) $ lines s, (x, c) <- l]

nextPos :: Coord -> Dir -> Coord
nextPos (x0, y0) dir =
    case dir of
        North -> (x0, pred y0)
        South -> (x0, succ y0)
        East -> (succ x0, y0)
        West -> (pred x0, y0)

nextDir srcDir c =
    case c of
        '-' -> srcDir
        '|' -> srcDir
        'L' -> if srcDir == South then East else North
        'J' -> if srcDir == South then West else North
        '7' -> if srcDir == North then West else South
        'F' -> if srcDir == North then East else South
        _ -> error ("Impossible tile: " ++ [c])
