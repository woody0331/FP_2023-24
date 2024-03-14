main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter 34526 == 20 --my test

sumDigitsIter :: Int -> Int
sumDigitsIter n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper k result = helper (div k 10) (result + mod k 10)