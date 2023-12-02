{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import qualified Data.Text.Read as TR (decimal)

data Color = Red | Green | Blue

data Game = Game {no :: Int, red :: [Int], green :: [Int], blue :: [Int]}

main :: IO ()
main = TIO.interact solution

solution :: Text -> Text
solution t = toStrict $ toLazyText ("Part1: " <> p1 <> "\n" <> "Part2: " <> p2 <> "\n" :: Builder)
  where
    ls = T.lines t
    games = map parseGame ls
    p1 = decimal $ part1 games
    p2 = decimal $ part2 games

part1 :: [Game] -> Int
part1 = sum . map no . filter possible

part2 :: [Game] -> Int
part2 = sum . map gamePower

gamePower :: Game -> Int
gamePower (Game _ rs gs bs) = product [maximum rs, maximum gs, maximum bs]

parseGame :: Text -> Game
parseGame t = case TR.decimal $ T.drop 5 t of
    Left _ -> error "error parsing game"
    Right (n, rest) -> case parseHands $ T.drop 2 rest of
        (rs, gs, bs) -> Game n rs gs bs

parseHands :: Text -> ([Int], [Int], [Int])
parseHands = foldl' accFun ([], [], []) . concatMap (hands . T.split (',' ==)) . T.split (';' ==)
  where
    accFun (rs, gs, bs) (n, c) = case c of
        Red -> (n : rs, gs, bs)
        Green -> (rs, n : gs, bs)
        Blue -> (rs, gs, n : bs)
        
    hands = map (\t -> (cnt t, color t))

    cnt :: Text -> Int
    cnt = either (error "no count") fst . TR.decimal . T.takeWhile isDigit . T.dropWhile isSpace

    color t = case T.last $ T.stripEnd t of
        'd' -> Red
        'n' -> Green
        'e' -> Blue
        _ -> error ":O"

possible :: Game -> Bool
possible (Game _ rs gs bs) =
    let
        maxR = maximum rs
        maxG = maximum gs
        maxB = maximum bs
     in
        maxR <= 12 && maxG <= 13 && maxB <= 14
