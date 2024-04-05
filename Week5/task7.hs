main :: IO()
main = do
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30
    print $ (switchSum (\x -> x + 2) (\x -> x * 3) 5) 4 == 166 --my test

switchSum :: (Num a) => (a -> a) -> (a -> a) -> Int -> (a -> a)
switchSum f _ 1 = (\x -> f x)
switchSum f g n = (\x -> f x + switchSum g f (n - 1) (f x))