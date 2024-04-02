main :: IO()
main = do
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 -- == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 -- == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 -- == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 -- == 30

switchSum :: (Num a) => (a -> a) -> (a -> a) -> Int -> (a -> a)
switchSum f g n = \y -> sum $ take n $ iterate (\x -> g (f x)) y