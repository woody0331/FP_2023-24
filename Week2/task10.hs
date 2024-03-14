main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11
    print $ countPalindromes 1 100 == 17 --my test 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99

countPalindromes :: Int -> Int -> Int
countPalindromes s f = helper ((min s f) + 1) ((max s f) - 1) 0
 where
    helper :: Int -> Int -> Int -> Int
    helper start finish count
     | start > finish = count
     | isPalindrome start == True = helper (start + 1) finish (count + 1)
     | otherwise = helper (start + 1) finish count

rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper n result
     | n == 0 = result
     | otherwise = helper (div n 10) (result * 10 + mod n 10)


isPalindrome :: Int -> Bool
isPalindrome n
 | n < 0 = error "The number has to be non-negative"
 | otherwise = n == rev n