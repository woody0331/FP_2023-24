import Data.List
main :: IO()
main = do
    print $ maxChain [(3, 4), (5, 6), (7, 8)] == 3
    print $ maxChain [(9, 14), (3, 5), (4, 7)] == 2

maxChain :: [(Int,Int)] -> Int
maxChain xs = 1 + (maximum 
    $ map (\ perms -> length $ takeWhile(\((_,x), (y,_)) -> x<y) 
    $ zip perms (tail perms)) permss)
 where 
    permss = permutations xs 