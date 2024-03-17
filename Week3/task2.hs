main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

countDigits :: Int -> Int
countDigits n
 | n < 10 = 1
 | otherwise = 1 + countDigits (div n 10)

sortN :: Int -> Int
sortN n = helper n 0 (countDigits n)
 where
    helper :: Int -> Int -> Int -> Int
    helper n result pow
     | pow == 0 = result
     | otherwise = helper (removeFirstOccurrence n (findMax n)) (result + (findMax n) * (10 ^ (pow-1))) (pow - 1)

findMax :: Int -> Int
findMax n = helperPM n 0 
 where
    helperPM :: Int -> Int -> Int
    helperPM 0 currentMax = currentMax
    helperPM leftover currentMax =
        helperPM (div leftover 10)
            (max (mod leftover 10) currentMax)

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence number digit = helper digit 0 0 number
 where
    helper :: Int -> Int -> Int -> Int -> Int
    helper d pos result leftover
     | leftover <= 0 = result
     | mod leftover 10 == d = (div number (10^(pos + 1))) * 10^pos + (mod number (10^pos))
     | otherwise = helper d (pos + 1) number (div leftover 10)