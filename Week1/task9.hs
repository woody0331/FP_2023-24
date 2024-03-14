main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10 -- upSpeed=100, downSpeed=10, desiredHeight=910
    print $ growingPlant 18 14 22 == 2 --my test

growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight
 | upSpeed >= desiredHeight = 1
 | otherwise = 1 + growingPlant upSpeed downSpeed (desiredHeight + downSpeed - upSpeed)