import Data.Char
main :: IO()
main = do
    print $ checkNumber 2728 == (4,15)
    print $ checkNumber 31415 == (12,2)
    print $ checkNumber 121 == (2,2)
    print $ checkNumber 141121 == (4,6) --mytest

checkNumber :: Int -> (Int, Int)
checkNumber xs = (sumEven, sumOdd)
 where
    sumEven = sum $ map digitToInt $ map snd $ filter (even . fst) $ zip [0..] $ show xs
    sumOdd = sum $ map digitToInt $ map snd $ filter (odd . fst) $ zip [0..] $ show xs 