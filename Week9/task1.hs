import Data.List
main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0
    print $ (sumExpr (/2) [2, 4, 6, 8, 10]) 10 == 543210.0 -- my test

sumExpr :: (Num a )=> (a -> a) -> [a] -> (a -> a)
sumExpr f ys = (\x -> sum [y * f (x ^ n) | (y, n) <- zip ys [1 .. ]])