main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.4, 157.08, 125.66, 62.83]
    print $ getVolumes [(13, 9), (23, 5), (8, 8), (12, 15)] == [4778.36,8309.51,1608.5,6785.84] -- my test

type Cylinder = (Double, Double)

getVolumes :: [Cylinder] -> [Double]
getVolumes cylinder = [(fromIntegral $ round $ (pi * r**2 * h) * 100) / 100 | (r,h) <- cylinder]
 where pi = 3.14159265359