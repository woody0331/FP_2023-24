main :: IO()
main = do
    print $ sumDivs 0 == 0
    print $ sumDivs 1 == 1
    print $ sumDivs 6 == 12 -- 1 + 2 + 3 + 6
    print $ sumDivs 12345 == 19776

sumDivs :: Int -> Int
sumDivs n = helper 0 1
 where
    helper :: Int -> Int -> Int
    helper result divisor
     | divisor > n = result
     | mod n divisor == 0 = helper (result + divisor) $ divisor + 1
     | otherwise = helper result $ divisor + 1 