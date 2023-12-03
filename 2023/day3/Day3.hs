import Control.Applicative
import Control.Monad (when)
import Data.Char (isDigit)
import Data.List (foldl', nub, singleton)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP

data Element = Number Int | Symbol deriving (Show)
type Consumed = Int
type Pos = Int
type NewPos = Int
type Coord = (Pos, Pos)
type Schematic = M.Map Coord Int

-- Absolutely hacky mess, I didn't have time to clean up :(

newtype ParseState = PS (NewPos, [(Pos, Element)]) deriving (Show)

instance Semigroup ParseState where
    (<>) (PS (lpos, lst)) (PS (rpos, rst)) = PS (rpos, lst <> rst)

instance Monoid ParseState where
    mempty = PS (0, [])

getPos :: ParseState -> NewPos
getPos (PS (pos, _)) = pos

getState :: ParseState -> [(Pos, Element)]
getState (PS (_, st)) = st

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\n" ++ "Part2: " ++ show p2 ++ "\n"
  where
    ls = lines s
    schematic = parseField $ lines s
    p1 = part1 schematic ls
    p2 = part2 schematic ls

part1 :: Schematic -> [String] -> Int
part1 schem xs = sum $ foldl' (f schem) [] withCoords
  where
    withCoords = concat $ zipWith (\y s -> zipWith (addCoord y) [0 ..] s) [0 ..] xs
    addCoord y x c = ((x, y), c)
    f m acc (pos, c) =
        if isSymbol c
            then let coords = surroundCoords pos in nub (mapMaybe (`M.lookup` m) coords) ++ acc
            else acc

part2 :: Schematic -> [String] -> Int
part2 schem xs = sum $ foldl' (f schem) [] withCoords
  where
    withCoords = concat $ zipWith (\y s -> zipWith (addCoord y) [0 ..] s) [0 ..] xs
    addCoord y x c = ((x, y), c)
    f m acc (pos, c) =
        if c == '*'
            then
                let coords = surroundCoords pos
                    ratios = nub (mapMaybe (`M.lookup` m) coords)
                 in if length ratios == 2 then product ratios : acc else acc
            else acc

surroundCoords :: Coord -> [Coord]
surroundCoords (x0, y0) =
    [ (x, y)
    | x <- [pred x0 .. succ x0]
    , x >= 0
    , y <- [pred y0 .. succ y0]
    , y >= 0
    , x /= x0 || y /= y0
    ]

parseField :: [String] -> Schematic
parseField xs = M.unions $ map (uncurry ingestLine) withYPos
  where
    withYPos = zip [0 ..] xs
    toIntList = mapMaybe (\(p, el) -> case el of Number i -> Just (p, i); _ -> Nothing)
    ingestLine ypos = M.fromList . map (addYPos ypos) . toIntList . getState . fst . head . readP_to_S parseLine
    addYPos ypos (xpos, elmt) = ((xpos, ypos), elmt)

-- >>> readP_to_S parseLine "467..114.."
parseLine :: ReadP ParseState
parseLine = rest mempty
  where
    rest ps = (eof >> return ps) <++ let tmp = parseElement ps in tmp >>= rest

-- >>> readP_to_S (parseElement mempty) "467..114.."
parseElement :: ParseState -> ReadP ParseState
parseElement ps = do
    let xpos = getPos ps
    res <- number <++ symbol <++ dots
    return $
        ps
            <> case res of
                Right e@(Number i) -> let len = numDigits i in PS (xpos + len, [(x, e) | x <- [xpos .. (xpos + len - 1)]])
                Right Symbol -> PS (succ xpos, [(xpos, Symbol)])
                Left consumed -> PS (xpos + consumed, [])

dots :: ReadP (Either Consumed Element)
dots = do
    ds <- many1 (char '.')
    rest <- look
    when (not (null rest) && '.' == head rest) pfail
    return $ Left $ length ds

number :: ReadP (Either Consumed Element)
number = do
    ds <- many1 $ satisfy isDigit
    rest <- look
    when (not (null rest) && isDigit (head rest)) pfail
    return $ Right (Number $ read ds)

symbol :: ReadP (Either Consumed Element)
symbol = Right Symbol <$ satisfy isSymbol

isSymbol :: Char -> Bool
isSymbol c = not (isDigit c) && c /= '.'

numDigits :: Int -> Int
numDigits i = length $ show i
