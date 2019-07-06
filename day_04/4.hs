import           Data.List.Split (splitOn)
import           Data.List (sort)
import           Data.Maybe (fromJust)
import qualified Data.Map as Map

type Guard  = Map.Map Int Int
type Guards = Map.Map Int Guard

maxSnd :: (Ord a, Num a) => [(a, (a, a))] -> (a, (a, a))
maxSnd list = foldl1 (\acc x -> if snd (snd x) > snd (snd acc) then x else acc) list

maxValue :: (Ord a, Num a) => Map.Map k a -> (k, a)
maxValue map = foldl1 (\(accK, accV) (k, v) -> if v > accV then (k,v) else (accK, accV)) $ Map.toList map

findGuard :: Guards -> (Int, Guard)
findGuard guards = foldl1 (\accGuard guard -> if sum (snd guard) > sum (snd accGuard) then guard else accGuard) $ Map.toList guards

parse :: [String] -> [(Int, String)]
parse []           = []
parse (entry:rest) = (time, action) : parse rest
    where cut    = words entry
          time   = read $ take 2 $ drop 3 $ cut !! 1
          action = cut !! 3

updateTime :: Int -> Int -> Guard -> Guard
updateTime start end guard = newGuard
    where newGuard = foldl (\acc x -> Map.insertWith (+) x 1 acc) guard [start..end-1]

convert :: Maybe Int -> Maybe Int -> [(Int, String)] -> Guards -> Guards
convert _ _ [] guards = guards
convert id asleepTime ((time, action):rest) guards
    | head action == '#' = convert readId Nothing rest guards
    | action == "asleep" = convert id (Just time) rest guards
    | action == "up"     = convert id Nothing rest (Map.insert (fromJust id) updatedGuard guards)
    where readId       = Just (read (tail action))
          updatedGuard = updateTime (fromJust asleepTime) time $ Map.findWithDefault Map.empty (fromJust id) guards

main = do
    x <- readFile "4.in"

    let guards = convert Nothing Nothing (parse $ sort $ lines x) Map.empty
    let laziestGuard = findGuard guards

    print "Part1"
    print $ fst laziestGuard * fst (maxValue $ snd laziestGuard)

    let p2 = maxSnd $ map (\x -> (fst x, maxValue $ snd x)) $ Map.toList guards
    
    print "Part2"
    print $ fst p2 * fst (snd p2)