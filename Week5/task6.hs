main :: IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 -- == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2), (+3)]) 1 -- == 8

pairCompose :: [a -> a] -> (a -> a)
pairCompose [] = id
pairCompose (f:fs) = \x -> (pairCompose fs) (f x)