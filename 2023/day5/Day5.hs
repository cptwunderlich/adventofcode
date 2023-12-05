import Data.Char (isDigit)
import Data.List (foldl')

data Range = Range {rDst :: Int, rSrc :: Int, rLen :: Int} deriving (Show)

type Seeds = [Int]
type Maps = [[Range]]

main :: IO ()
main = interact solution

solution :: String -> String
solution s = "Part1: " ++ show p1 ++ "\nPart2: " ++ show p2 ++ "\n"
  where
    (seeds, maps) = parse s
    p1 = part1 seeds maps
    p2 = 0 :: Int

part1 :: Seeds -> Maps -> Int
part1 seeds maps = minimum $ foldl' translate seeds maps
  where
    translate seeds r = map (translateSeed r) seeds

translateSeed :: [Range] -> Int -> Int
translateSeed rs s = go s rs
  where
    go s [] = s
    go s (r : rs) = if matches s r then rDst r + (s - rSrc r) else go s rs
    matches s (Range dst src len) = src <= s && s < (src + len)

parse :: String -> (Seeds, Maps)
parse s =
    let (h : ls) = lines s
        seeds = parseSeeds h
        maps = parseMap ls
     in (seeds, maps)

parseSeeds :: String -> [Int]
parseSeeds = map read . words . dropWhile (not . isDigit)

parseMap :: [String] -> Maps
parseMap = reverse . go []
  where
    go acc [] = acc
    go acc rest =
        let start = toStart rest
            acc' = block start : acc
         in go acc' $ dropWhile startsWithDigit start
    toStart = dropWhile (not . startsWithDigit)
    block =
        map (mkRange . map read . words)
            . takeWhile startsWithDigit

mkRange :: [Int] -> Range
mkRange [d, s, l] = Range d s l
mkRange x = error $ "Invalid input: " ++ show x

startsWithDigit :: String -> Bool
startsWithDigit "" = False
startsWithDigit s = isDigit (head s)
