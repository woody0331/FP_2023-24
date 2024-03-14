main :: IO()
main = do
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3
    print $ countDigitsIter 564367 == 6 --my test

    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3
    print $ countDigitsRec 564367 == 6 --my test

countDigitsIter :: Int -> Int
countDigitsIter n
 | n < 10 = 1
 | otherwise = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 count = count
    helper k count = helper (div k 10) (count + 1)

countDigitsRec :: Int -> Int
countDigitsRec n
 | n < 10 = 1
 | otherwise = 1 + countDigitsRec (div n 10)
