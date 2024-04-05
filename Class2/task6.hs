main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True -- 1 + 2 + 3 = 6 = 6
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True

isPerfect :: Int -> Bool
isPerfect n = n == sumDivs n - n

sumDivs :: Int -> Int
sumDivs n = helper 0 1
 where
    helper :: Int -> Int -> Int
    helper result divisor
     | divisor > n = result
     | mod n divisor == 0 = helper (result + divisor) $ divisor + 1
     | otherwise = helper result $ divisor + 1