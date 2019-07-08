import           Data.Char (isUpper, toLower, toUpper)
import           Data.List (nub)
import qualified Data.Map as Map

switchCase :: Char -> Char
switchCase a
    | isUpper a = toLower a
    | otherwise = toUpper a

reduct :: String -> String -> String
reduct stack []    = stack
reduct [] (x:rest) = reduct [x] rest
reduct stack (x:rest)
    | last stack == switchCase x = reduct (init stack) rest
    | otherwise                  = reduct (stack ++ [x]) rest

removeLetter :: Char -> String -> String
removeLetter letter x = filter (\a -> toUpper a /= letter) x

main = do
    x <- readFile "5.in"

    print $ "Part1"
    let reducted = reduct "" x
    print $ length $ reducted

    let letters = nub $ map toUpper x

    print $ "Part2"
    print $ minimum $ map (\char -> length $ reduct "" $ removeLetter char reducted) letters