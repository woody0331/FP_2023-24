main :: IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = helper (min start finish) (max start finish) k 0
 where
    helper :: Int -> Int -> Int -> Int -> Int
    helper start finish k result
     | start > finish = result
     | mod (sumDigits start) k == 0 = helper (start + 1) finish k (result + start)
     | otherwise = helper (start + 1) finish k (result)

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits n = mod n 10 + sumDigits (div n 10)
