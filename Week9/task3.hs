import Data.List
main :: IO()
main = do
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)

    print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)

    print $ getClosestDistance [(TwoD 23 43), (TwoD 52 130), (TwoD 52 129), (TwoD 132 45), (TwoD 34 22), (TwoD 69 42)] == (1.0,TwoD 52.0 130.0,TwoD 52.0 129.0) --my test
    print $ getClosestDistance [(ThreeD 1 5 6), (ThreeD 2 5 10), (ThreeD (-22) 1 45), (ThreeD 122 0 2)] == (4.123105625617661,ThreeD 1.0 5.0 6.0,ThreeD 2.0 5.0 10.0) --my test

data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq)

getClosestDistance :: (Floating a, Ord a) => [Point a] -> (a, Point a, Point a)
getClosestDistance [] = error "The list cannot be empty"
getClosestDistance [_] = error "Need at least two points to find closest distance"
getClosestDistance (p1:p2:ps) = foldl findMinDistance (distance p1 p2, p1, p2) pairs
 where
    pairs = [(distance p1 p2, p1, p2) | (p1, i1) <- zip (p1:p2:ps) [0..], (p2, i2) <- zip (p1:p2:ps) [0..], i1 < i2]
    findMinDistance clstDist@(minDist, _, _) (dist, p1, p2)
     | dist < minDist = (dist, p1, p2)
     | otherwise = clstDist

distance :: Floating a => Point a -> Point a -> a
distance (TwoD x1 y1) (TwoD x2 y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2
distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2
distance _ _ = error "Not implemented!"

