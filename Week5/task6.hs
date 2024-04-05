main :: IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8
    print $ (pairCompose [(+2), (+3), (+4), (+5)]) 1 == 17 --my test

pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] = id
pairCompose [f1] = f1
pairCompose (f1:f2:xs) = (\x -> f1 (f2 x) + pairCompose xs x)

