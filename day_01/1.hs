import qualified Data.Set as Set

getFrequencies :: String -> [Int]
getFrequencies = map (read . dropWhile (=='+')) . lines

findDuplicate :: Int -> Set.Set Int -> [Int] -> Int
findDuplicate x values (i:instructions)
    | x `Set.member` values = x
    | otherwise             = findDuplicate (x + i) (Set.insert x $ values) instructions

main = do
    x <- readFile "1.in"
    
    print "Part 1"
    print $ sum $ getFrequencies x

    print "Part 2"
    print $ findDuplicate 0 (Set.fromList []) (cycle $ getFrequencies x)