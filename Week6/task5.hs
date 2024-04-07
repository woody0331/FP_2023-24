main :: IO()
main = do
    print $ (line (5,1) (1,4)) 3 == 2.5
    print $ liesOn (line (10, 2) (0, 1)) (2.5, (line (5, 1) (1, 4)) 3) == False 

type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1,y1) (x2,y2) = (\x -> y1 + (x - x1) * (y2-y1) / (x2 - x1))

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = (\ (x,y) -> y == f x)