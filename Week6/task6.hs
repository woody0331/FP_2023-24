import Data.Char
import Data.List
main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2
    print $ (getOddCompositionValue [(\x -> x + 2),(\x -> x * 3),(\x -> x - 4), (\x -> div x 4)]) 3 == 1 --my test

getOddCompositionValue :: [(Integer -> Integer)] -> (Integer -> Integer)
getOddCompositionValue fs = foldr1 (.) $ map snd $ filter (odd . fst) $ zip [1.. ] fs
