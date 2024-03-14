main :: IO()
main = do
    print $ maxMultiple 2 7 == 6
    print $ maxMultiple 3 10 == 9
    print $ maxMultiple 7 17 == 14
    print $ maxMultiple 10 50 == 50
    print $ maxMultiple 37 200 == 185
    print $ maxMultiple 7 100 == 98  
    print $ maxMultiple 7 10 == 7
    print $ maxMultiple 4 4 == 4
    print $ maxMultiple 29 94 == 87 --my test

maxMultiple :: Int -> Int -> Int
maxMultiple d b = helper d b b
 where
    helper :: Int -> Int -> Int -> Int
    helper d b n
     | mod n d == 0 && n <= b && n > 0 = n
     | otherwise = helper d b (n-1)