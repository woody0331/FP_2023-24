main :: IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False
    print $ isArithmetic [3, 5, 9, 9, 11] == False
    print $ isArithmetic [16, 19, 22, 25, 28, 31] == True --my test

isArithmetic :: [Int] -> Bool
isArithmetic xs = all (\i -> xs !! (i + 1) - xs !! i == xs !! (i + 2) - xs !! (i + 1)) [0 .. length xs - 3] 
--lenght - 1 е индексът на последния елемент, за последната проверка са ни нужни два "скока" значи lenght -3