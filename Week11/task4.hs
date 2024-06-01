import Data.List
main:: IO()
main = do
    print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 21
    print $ closestAverage [(Temp 1 12.2), (Temp 7 17.1), (Temp 8 19.0), (Temp 10 15.8), (Temp 24 17.4), (Temp 27 16.3), (Temp 29 19.7)] == 7 --mytest

data Measuring = Temp Int Float

closestAverage :: [Measuring] -> Int
closestAverage [] = error "Empty list"
closestAverage xs = fst $ foldr (\(Temp day temp) (closestDay, closestTemp) -> if abs (temp - avg) < abs (closestTemp - avg) then (day, temp) else (closestDay, closestTemp)) (error "Empty list", 0) xs
 where 
    avg = sum [temp | Temp _ temp <- xs] / fromIntegral (length xs)