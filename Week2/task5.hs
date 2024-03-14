main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    print $ areAmicable 10744 10856 == True --my test

areAmicable :: Int -> Int -> Bool
areAmicable n k = sumDivs n == sumDivs k

sumDivs :: Int -> Int
sumDivs n = helper 0 1
 where
    helper :: Int -> Int -> Int
    helper result divisor
     | divisor > n = result
     | mod n divisor == 0 = helper (result + divisor) $ divisor + 1
     | otherwise = helper result $ divisor + 1 