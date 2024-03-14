main :: IO()
main = do
    print $ subNum 123 5123783 == True -- x = 123, y = 5123783
    print $ subNum 0 0 == True
    print $ subNum 10 101 == True
    print $ subNum 101 101 == True
    print $ subNum 10 0 == False
    print $ subNumOneLine 1253 5123783 == False
    print $ subNum 12 0 == False

subNum :: Int -> Int -> Bool
subNum x y
 | x > y = False
 | x == mod y (10 ^ (countDigits x)) = True
 | otherwise = subNum x (div y 10)

subNumOneLine :: Int -> Int -> Bool
subNumOneLine x y = x <= y && (x == mod y (10 ^ (countDigits x)) || subNumOneLine x (div y 10))

countDigits :: Int -> Int
countDigits n
 | n < 10 = 1
 | otherwise = 1 + countDigits (div n 10)