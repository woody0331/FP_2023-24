main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51
    print $ p 7 == 70
    print $ p 8 == 92
    print $ p 9 == 117
    print $ p 10 == 145

p :: Int -> Int
p n = div (n * (3 * n - 1)) 2