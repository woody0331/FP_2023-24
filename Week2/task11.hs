main :: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not
    print $ truncatablePrime 0 == False
    print $ truncatablePrime 1 == False
    print $ truncatablePrime 2 == True
    print $ truncatablePrime 37397 == True
    print $ truncatablePrime 1399 == False -- 1 is not prime
    print $ truncatablePrime 1733 == False -- 1 is not prime
    print $ truncatablePrime 1913 == False -- 1 is not prime
    print $ truncatablePrime 1931 == False -- 1 is not prime
    print $ truncatablePrime 1933 == False -- 1 is not prime
    print $ truncatablePrime 1973 == False -- 1 is not prime
    print $ truncatablePrime 19333 == False -- 1 is not prime
    print $ truncatablePrime 19739 == False -- 1 is not prime
    print $ truncatablePrime 5939 == True --my test

truncatablePrime :: Int -> Bool
truncatablePrime n
 | n <= 10 && isPrime n = True
 | isPrime n = truncatablePrime (div n 10)
 | otherwise = False

isPrime :: Int -> Bool
isPrime n = n > 1 && helper n 2
 where
    helper n divisor
     | divisor >= n = True
     | mod n divisor == 0 = False
     | otherwise = helper n (divisor + 1)