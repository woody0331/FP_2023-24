main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6
    print $ mySumRecNonPM [4, 5, 6, 7] == 22 --my test
    
    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6
    print $ mySumRecPM [4, 5, 6, 7] == 22 --my test

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6
    print $ mySumFunc [4, 5, 6, 7] == 22 --my test

mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs
 | null xs = 0
 | otherwise = head xs + (mySumRecNonPM $ tail xs)

mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

mySumFunc :: [Int] -> Int
mySumFunc = sum