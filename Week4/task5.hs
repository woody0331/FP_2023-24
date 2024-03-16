main :: IO()
main = do
    print $ isAscending 0 == True
    print $ isAscending 10 == False
    print $ isAscending 123 == True
    print $ isAscending 1233 == True
    print $ isAscending 12332 == False
    print $ isAscending 1234567 == True --my test


isAscending :: Int -> Bool
isAscending n = helper (show n)
helper [_] = True
helper (x0:x1:xs) = x0 <= x1 && helper (x1:xs)
