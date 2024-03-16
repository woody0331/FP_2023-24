main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    print $ sumSpecialPrimes 14 1 == 924 --my test

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum( take n $ filter (\x -> isPrime x && elem (head (show d)) (show x)) [1 ..])

isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n]