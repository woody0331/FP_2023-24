main :: IO()
main = do
    print $ countOccurrences 121 1 == 2
    print $ countOccurrences 222 1 == 0
    print $ countOccurrences 100 0  == 2
    print $ countOccurrences 0 0 == 1
    print $ countOccurrences 352353 3 == 3 --my test

countOccurrences :: Int -> Int -> Int
countOccurrences n k 
 | n == 0 && k == 0 = 1
 | otherwise = helper n k 0
 where
    helper 0 _ count = count
    helper n k count
     | mod n 10 == k = helper (div n 10) k (count + 1)
     | otherwise = helper (div n 10) k (count)


