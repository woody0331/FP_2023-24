main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True
    print $ isPresentRecNonPM 3 [0,-4, 1, 5, 3, 2, -1, 2] == True --my test

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True
    print $ isPresentRecPM 3 [0,-4, 1, 5, 3, 2, -1, 2] == True --my test

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True
    print $ isPresentFunc 3 [0,-4, 1, 5, 3, 2, -1, 2] == True --my test

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs
 | null xs = False
 | head xs == n = True
 | otherwise = isPresentRecNonPM n (tail xs)

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM n [] = False
isPresentRecPM n (x:xs)
 | x == n = True
 | otherwise = isPresentRecPM n xs

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc n [] = False
isPresentFunc n (x:xs) = elem n (x:xs)