main ::IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ getAreas [Circle 6, Rectangle 6 4, Rectangle 7 3, Triangle 6 8 10, Cylinder 2 6] == [113.09733552923255,24.0,21.0,24.0,100.53096491487338] --my test
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0
    print $ maxArea [Circle 6, Rectangle 6 4, Rectangle 7 3, Triangle 6 8 10, Cylinder 2 6] == Circle 6 --my test

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq, Read, Ord)

area :: Floating a => Shape a -> a


getAreas :: Floating a => [Shape a] -> [a]


maxArea :: (Ord a, Floating a) => [Shape a] -> Shape a
