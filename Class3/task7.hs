main :: IO()
main = do
    print $ mySin 10 1 == 0.8414709848078965 -- n = 100, x = 1
    print $ mySin 15 0.5 == 0.479425538604203 -- n = 100, x = 1
    -- print $ mySin 100 1 -- == 0.8414709848078965 -- n = 100, x = 1
    -- print $ mySin 100 0.5 -- == 0.479425538604203

factIter :: Int -> Int
factIter n = helper n 1
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (leftover - 1) (result * leftover)

mySin :: Int -> Double -> Double
mySin 0 x = x
mySin n x = ((-1)^n * x**(fromIntegral $ 2 * n + 1)) / (fromIntegral $ factIter $ 2 * n + 1) + mySin (n - 1) x