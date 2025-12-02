data Range = Range Int Int deriving Show

parse :: String -> [Range]
parse s = go [] s
    where
        toRange s = Range (read $ takeWhile ('-' /=) s) (read $ drop 1 $ dropWhile ('-' /=) s)
        go acc "" = acc
        go acc s  = let (match, rest) = span (',' /=) s
                    in  go (toRange match : acc) $ drop 1 rest

part1 :: [Range] -> String
part1 = show . foldl' f 0
    where
        numDigits :: Int -> Int
        numDigits x = 1 + truncate (logBase 10 (fromIntegral x))
        f :: Int -> Range -> Int
        f acc (Range from to) =
            if odd (numDigits from) && odd (numDigits to) && numDigits from == numDigits to
            then acc
            else acc + sum (process from to)
        upper :: Int -> Int -> Int
        upper pid digits = truncate $ fromIntegral pid / (10**(fromIntegral digits/2))
        lower :: Int -> Int -> Int
        lower pid digits = pid `rem` truncate (10**(fromIntegral digits/2))
        invalidId :: Int -> Bool
        invalidId pid    = let digits = numDigits pid
                           in even digits && upper pid digits == lower pid digits
        process from to = [x | x <- [from..to], invalidId x]

main :: IO ()
main = interact (part1 . parse)

