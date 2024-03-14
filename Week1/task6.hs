main :: IO()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13
    print $ myGcdG 182 77 == 7 --my test

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13
    print $ myGcdPM 182 77 == 7 --my test

myGcdG :: Int -> Int -> Int
myGcdG x y
 |x == 0 = y
 |y == 0 = x 
 |x == y = x
 |otherwise = myGcdG y (mod x y)

myGcdPM :: Int -> Int -> Int
myGcdPM x 0 = x
myGcdPM 0 y = y
myGcdPM x y = myGcdG y (mod x y)