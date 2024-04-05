main :: IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1
    print $ (applyN (\x -> mod x 17) 5) 1341 == 15 --my test

applyN :: (a -> a) -> Int -> (a -> a)
applyN f n = (\x -> iterate f x !! n)