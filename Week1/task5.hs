main :: IO()
main = do
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5
    print $ sqAvg 8 9 == 72.5 --my test

sqAvg :: Int -> Int -> Float
sqAvg x y = (fromIntegral $ x^2 + y^2) / 2