main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789
    print $ rev 323241 == 142323 --my test

rev :: Int -> Int
rev n
 | n < 0 = error "Must be non-negative number"
 | n < 10 = n
 | otherwise = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper k result = helper (div k 10) (result * 10 + mod k 10)