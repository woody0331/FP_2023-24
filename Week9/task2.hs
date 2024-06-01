main :: IO()
main = do
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 (-16)), (City "Plovdiv" 120 34), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 (-15)), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Vratsa" 344 9), (City "Montana" 147 17), (City "Vidin" 43 13)]), (Country "Germany" "Berlin" [(City "Dortmund" 86 (-5)), (City "Darmstadt" 144 2), (City "Wolfsburg" 63 1)]), (Country "Italy" "Rome" [(City "Rome" 21 15), (City "Turin" 239 14), (City "Genoa" 20 20)])] == "Germany" --my test

type Name = String
type Capital = Name 
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)

coldestCapital :: [Country] -> Name
coldestCapital countries = fst $ foldl findColdest countryByTemp $ tail zipped
 where
    avgTemps = map (\(Country _ _ cities) -> average (map (\(City _ _ temp) -> temp) cities)) countries
    zipped = zip (map (\(Country name _ _) -> name) countries) avgTemps
    countryByTemp = head zipped

findColdest :: (Name, AvgYearlyTemperature) -> (Name, AvgYearlyTemperature) -> (Name, AvgYearlyTemperature)
findColdest (coldestCountry, coldestTemp) (country, temp)
 | temp < coldestTemp = (country, temp)
 | otherwise = (coldestCountry, coldestTemp)