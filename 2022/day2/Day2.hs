main :: IO ()
main = interact solution

data Hand = Rock | Paper | Scissors deriving (Enum, Eq)

solution :: String -> String
solution s = show p1 ++ "\n" ++ show p2 ++ "\n"
  where
    round s = case words s of
              [x, y] -> (toValue x, toValue y)
              _ -> error "err"
    rounds  = map round $ lines s
    p1      = sumScore rounds
    rounds2 = map (\s -> case words s of { [x, y] -> (toValue x, makeMove (toValue x) y) }) $ lines s
    p2      = sumScore rounds2

sumScore :: [(Hand, Hand)] -> Int
sumScore = sum . map score

score :: (Hand, Hand) -> Int
score (other, me)
  | other == me = myVal + 3
  | otherwise =
      case (other, me) of
          (Rock, Paper) -> myVal + 6
          (Rock, Scissors) -> myVal
          (Paper, Rock) -> myVal
          (Paper, Scissors) -> myVal + 6
          (Scissors, Rock) -> myVal + 6
          (Scissors, Paper) -> myVal
  where
    myVal = fromEnum me + 1

toValue :: String -> Hand
toValue x
  | x `elem` ["A", "X"] = Rock
  | x `elem` ["B", "Y"] = Paper
  | x `elem` ["C", "Z"] = Scissors

makeMove :: Hand -> String -> Hand
makeMove other strat
  | strat == "X" = case other of
                     Rock -> Scissors
                     Paper -> Rock
                     Scissors -> Paper
  | strat == "Y" = other
  | strat == "Z" = case other of
                     Rock -> Paper
                     Paper -> Scissors
                     Scissors -> Rock
