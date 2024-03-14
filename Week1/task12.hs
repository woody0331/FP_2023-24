main :: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          -- == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    -- == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84
    print $ finalGrade 5.67 4 6 4.25 4.87 4 4 4.83 3.90 == 4.44 --my test



finalGrade :: Double -> Double -> Double -> 
              Double -> Double -> Double ->
              Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it
 | tk (avrHomework d1 d2 d3) (avrTest kz1 kz2) (avrTest kt1 kt2) / 2.0 + max (free kz1 kz2 iz) iz / 4.0 + max (free kt1 kt2 it) it / 4.0 < 2.0 = 2.00
 | otherwise = fromIntegral (round ((tk (avrHomework d1 d2 d3) (avrTest kz1 kz2) (avrTest kt1 kt2) / 2.0 + max (free kz1 kz2 iz) iz / 4.0 + max (free kt1 kt2 it) it / 4.0) * 100)) / 100

tk :: Double -> Double -> Double -> Double
tk homework kz kt = (homework / 4.0) + (3 * kz / 8.0) + (3 * kt / 8.0)

avrHomework :: Double -> Double -> Double -> Double
avrHomework d1 d2 d3 = (d1 + d2 + d3) / 3.0

avrTest :: Double -> Double -> Double
avrTest k1 k2 = (k1 + k2) / 2.0

free :: Double -> Double -> Double -> Double
free k1 k2 exam
 | k1 >= 4.00 && k2 >= 4.00 && (avrTest k1 k2) >= 4.50 = avrTest k1 k2
 | otherwise = exam



