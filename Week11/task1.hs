import Data.List
main :: IO ()
main = do
    print $ (myPoly [2.7, 3.0 ..] ) 2.2 3 == -0.4399999999999998
    print $ (myPoly [1.4, 2.1 ..] ) 0.2 4 == 19.562400000000004 -- mytest

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> product $ take y $ map (x-) xs)
-- myPoly xs = (\x y -> product $ take y [(x - xi) | xi <- xs])