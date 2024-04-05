main :: IO()
main = do
    print $ onlyArithmetic [[3], [1, 2, 3, 4, 5], [3, 5, 8, 9, 11]]  == [[3], [1, 2, 3, 4, 5]]
    print $ onlyArithmetic [[1, 2, 3, 5], [2, 5, 8, 11, 14], [2, 3, 3, 4, 5, 6], [1, 1, 2, 3, 5, 8, 13]] == [[2, 5, 8, 11, 14]]

onlyArithmetic :: [[Int]] -> [[Int]]
onlyArithmetic = filter isArithmetic

isArithmetic :: [Int] -> Bool
isArithmetic xs = all (\i -> xs !! (i + 1) - xs !! i == xs !! (i + 2) - xs !! (i + 1)) [0 .. length xs - 3] 