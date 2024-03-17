main :: IO()
main = do
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 == -0.6666666666666667
    print $ calcSeriesSum 1 2 == -1.2000000000000002
    print $ calcSeriesSum 1 3 -- == -1.047619047619048
    print $ calcSeriesSum 1 4 -- == -1.0814814814814817
    print $ calcSeriesSum 1 5 == -1.0753246753246755
    print $ calcSeriesSum 1 6 == -1.0762718762718764
    print $ calcSeriesSum 10 34 == -0.10607741100173662


calcSeriesSum :: Double -> Int -> Double
calcSeriesSum x n = helper x n 1 1 0 
 where
    helper :: Double -> Int -> Double -> Double -> Int -> Double
    helper x n denum multiplier pow 
     | pow > n = 0
     | otherwise = (-1) ^(pow+1) * (2^(pow+1)) * (x^pow) / (denum) + helper x n (denum*(multiplier+2)) (multiplier+2) (pow + 1) 
