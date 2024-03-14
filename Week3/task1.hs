main :: IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence number digit = helper digit 0 0 number
 where
    helper :: Int -> Int -> Int -> Int -> Int
    helper d pos result leftover
     | leftover <= 0 = result
     | mod leftover 10 == d = (div number (10^(pos + 1))) * 10^pos + (mod number (10^pos))
     | otherwise = helper d (pos + 1) number (div leftover 10)

