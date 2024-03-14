main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134
    print $ removeD 3 13333334 == 14 --my test

removeD :: Int -> Int -> Int
removeD d n = helper d n 0 0
 where
    helper :: Int -> Int -> Int -> Int -> Int
    helper d n result pow
     | n == 0 = result
     | mod n 10 /= d = helper d (div n 10) (result + (mod n 10) * 10^pow) (pow + 1)
     | otherwise = helper d (div n 10) result pow