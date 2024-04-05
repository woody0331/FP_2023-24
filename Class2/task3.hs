main :: IO()
main = do
    print $ powRec 2 5 == 32
    print $ powRec 15 3 == 3375

    print $ powIter 2 5 == 32
    print $ powIter 15 3 == 3375

powIter :: Double -> Int -> Double
powIter x n = helper n 1
 where
    helper :: Int -> Double -> Double
    helper 0 result = result
    helper leftover result = helper (leftover - 1) (result * x)

powRec :: Double -> Int -> Double
powRec _ 0 = 1
powRec x n = x * powRec x (n - 1)