import Prelude
main :: IO()
main = do
    print $ primesProd 9 == 2
    print $ primesProd 12 == 6
    print $ primesProd 1200 == 200560490130

--primesProd :: Int -> Int
primesProd x = helper 2 
 where
    helper d
     | d*d >= x = 1
     | isPrime d = d * helper (d+1)
     | otherwise = helper (d+1)


isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n]