main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"
    print $ highestCapital [(Country "Portugal" "Lisbon" [(City "Lisbon" 2 15), (City "Porto" 104 17), (City "Braga" 174 13)]), (Country "Germany" "Berlin" [(City "Dortmund" 86 (-5)), (City "Darmstadt" 144 2), (City "Wolfsburg" 63 1)]), (Country "Italy" "Rome" [(City "Rome" 21 15), (City "Turin" 239 14), (City "Genoa" 20 20)])] == "Portugal" --my test

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

highestCapital :: [Country] -> Name
highestCapital [] = error "There aren't any countries"
highestCapital countries = snd $ foldr findHighest (initialMax, initialName) countries
  where
    maxCityElevation cities = foldr (\(City _ el _) acc -> max el acc) 0 cities

    initialMax = maximum [el | Country _ _ cities <- countries, City _ el _ <- cities]
    initialName = let (Country name _ _) = head countries in name    

    findHighest (Country countryName _ cities) acc@(maxElevation, _) = 
         if highestCityElevation > maxElevation
         then (highestCityElevation, countryName)
         else acc
          where 
            highestCityElevation = maxCityElevation cities
