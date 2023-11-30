import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

findFirstRepeated :: IntSet -> Int -> [Int] -> Int
findFirstRepeated seen acc []   = error "No repetition - should never happen!"
findFirstRepeated seen acc (n:ns)
    | IntSet.member added seen  = added
    | otherwise = findFirstRepeated (IntSet.insert added seen) added (ns ++ [n])
    where added = acc + n 

main = do
            input <- readFile "./day1-input.txt"
            let nums =  fmap (read . dropWhile (== '+')) (lines input)
            print ("Sum: " ++ show (sum nums))
            let seen = IntSet.empty            
            print ("First repeated freqency: " ++ (show $ findFirstRepeated seen 0 nums))
            
