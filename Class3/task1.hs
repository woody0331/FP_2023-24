main :: IO()
main = do
    print $ countDigits 100 == 3
    print $ countDigits 42 == 2
    print $ sumDigitsWithPower 153 3 == 153
    print $ sumDigitsWithPower 1634 4 == 153
    print $ isNarcissistic 7 == True
    print $ isNarcissistic 12 == False
    print $ isNarcissistic 370 == True
    print $ isNarcissistic 371 == True
    print $ isNarcissistic 1634 == True

isNarcissistic :: Int -> Bool
isNarcissistic n = n == sumDigitsWithPower n (countDigits n)

sumDigitsWithPower :: Int -> Int -> Int
sumDigitsWithPower n power
 | n < 10 = n^power
 | otherwise = (mod n 10)^power + sumDigitsWithPower (div n 10) power

countDigits :: Int -> Int
countDigits n
 | n < 10 = 1
 | otherwise = 1 + countDigits (div n 10)