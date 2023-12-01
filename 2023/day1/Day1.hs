import Data.Char (isDigit)

main :: IO ()
main = interact solution

solution :: String -> String
solution s = show (part1 ls) ++ "\n" ++ show (part2 ls) ++ "\n"
  where
    ls = lines s
    part1 ls = sum $ map number ls
    part2 ls = part1 $ map replaceAll ls

number :: String -> Int
number s =
    let digits = filter isDigit s
        first = take 1 digits
        last = take 1 $ reverse digits
        num = if null digits then "0" else first ++ last
     in read num

replaceAll :: String -> String
replaceAll s = case replace s of
    (c : rest) -> c : replaceAll rest
    [] -> []

replace :: String -> String
replace s = case s of
    'o' : 'n' : 'e' : xs -> '1' : rest
    't' : 'w' : 'o' : xs -> '2' : rest
    't' : 'h' : 'r' : 'e' : xs -> '3' : rest
    'f' : 'o' : 'u' : 'r' : xs -> '4' : rest
    'f' : 'i' : 'v' : 'e' : xs -> '5' : rest
    's' : 'i' : 'x' : xs -> '6' : rest
    's' : 'e' : 'v' : 'e' : 'n' : xs -> '7' : rest
    'e' : 'i' : 'g' : 'h' : 't' : xs -> '8' : rest
    'n' : 'i' : 'n' : 'e' : xs -> '9' : rest
    _ -> s
  where
    rest = drop 1 s
