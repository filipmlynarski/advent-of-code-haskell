import           Data.List.Split (splitOn)
import qualified Data.Map as Map

type Cord  = Int
type Id    = Int
type Claim = (Id, Cord, Cord, Int, Int)
type Suit  = Map.Map (Cord, Cord) Int

parse :: [String] -> [Claim]
parse [] = []
parse (x:xs) = (id, x_cord, y_cord, width, height) : parse xs
    where first_part  = map read $ splitOn "," $ init $ (words x) !! 2
          second_part = map read $ splitOn "x" $ (words x) !! 3
          id     = read $ tail $ head $ words x
          x_cord = head first_part
          y_cord = last first_part
          width  = head second_part
          height = last second_part

combinations :: [a] -> [a] -> [(a, a)]
combinations [] _     = []
combinations (x:xs) y = (map (\l -> (x, l)) y) ++ combinations xs y

getCoordinates :: Claim -> [(Cord, Cord)]
getCoordinates (_, x_cord, y_cord, width, height) = combinations [x_cord..x_cord+width-1] [y_cord..y_cord+height-1]

sew :: Claim -> Suit -> Suit
sew claim suit = foldl (\acc x -> Map.insertWith (+) x 1 acc) suit $ getCoordinates claim

sewClaims :: [Claim] -> Suit -> Suit
sewClaims claims suit = foldl (\acc x -> sew x acc) suit claims

findClaim :: [Claim] -> Suit -> Maybe Id
findClaim [] _ = Nothing
findClaim ((id, x_cord, y_cord, width, height):claims) suit
    | all check_func check_cords = Just id
    | otherwise                  = findClaim claims suit
    where x_range     = [x_cord..x_cord+width-1]
          y_range     = [y_cord..y_cord+height-1]
          check_cords = combinations x_range y_range
          check_func  = (\cord -> Map.lookup cord suit == Just 1)

main = do
    x <- readFile "3.in"

    let claims = parse $ lines x
    let suit   = sewClaims claims Map.empty

    print "Part1"
    print $ length $ filter (>1) $ Map.elems suit

    print "Part2"
    print $ findClaim claims suit