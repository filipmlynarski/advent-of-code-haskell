import Data.List (group, sort)
import Data.Maybe (fromJust)

getIDs :: [[Int]] -> (Int, Int)
getIDs [] = (0, 0)
getIDs (x:xs) = 
    (two + (fst sub_IDs), three + (snd sub_IDs))
    where
        two     = if 2 `elem` x then 1 else 0
        three   = if 3 `elem` x then 1 else 0
        sub_IDs = getIDs xs

diff :: (String, String) -> Int
diff ([],[])          = 0
diff ((a:as), (b:bs)) = fromEnum (a /= b) + diff (as, bs)

blend :: String -> [String] -> [(String, String)]
blend _ []        = []
blend line (x:xs)
    | line /= x = (line, x) : (blend line xs)
    | otherwise = blend line xs

combine :: [String] -> [(String, String)]
combine lines = foldl (\acc x -> (blend x lines) ++ acc) [] lines

getFst :: (a -> Bool) -> [a] -> Maybe a
getFst _ [] = Nothing
getFst f (x:xs)
    | f x       = Just x
    | otherwise = getFst f xs

removeDiff :: (String, String) -> String
removeDiff ([], []) = ""
removeDiff ((a:as), (b:bs))
    | a == b    = a : removeDiff (as, bs)
    | otherwise = removeDiff (as, bs)

main = do
    x <- readFile "2.in"

    print "Part1"
    print $ uncurry (*) $ getIDs $ map (\x -> map length (group $ sort x)) $ lines x

    print "Part2"
    print $ removeDiff $ fromJust $ getFst (\x -> diff x == 1) $ combine $ lines x