main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98
    print $ findSum 3 7 6 == 772 --my test 108 + 220 + 444



findSum :: Int -> Int -> Int -> Int
findSum x y n
 | n < 3 = error "Must be 3 or more"
 | otherwise = helper x y (n-1) 0 + helper x y (n-2) 0 + helper x y (n-3) 0 
 where
    helper :: Int -> Int -> Int -> Int -> Int
    helper a b n result
     | n==0 = result + a*(n+1) + b*2^n
     | otherwise = helper a b (n - 1) (result + b*2^n)

