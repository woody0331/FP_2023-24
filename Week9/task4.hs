main :: IO()
main = do
    print $ findUncles t 5 == [3,4]
    print $ findUncles t 7 == [2,4]
    print $ findBrothers t 8 == [9] --testing if it works
    print $ findBrothers t 10 == [] --testing if it works
    print $ findUncles t 10 == [5]
    print $ findUncles t 8 == [2,3] --my test

type Tree = [(Int, [Int])]

t :: Tree
t = [(1,[2,3,4]),(2,[5,6]),(3,[7]),(4,[8,9]),(5,[]),(6,[10]),(7,[]),(8,[]),(9,[]),(10,[])]

findUncles :: Tree -> Int -> [Int]
findUncles tree node = findBrothers tree parent
 where
    parent = findParent node tree

findParent :: Int -> Tree -> Int
findParent _ [] = 0
findParent node ((parent, children):tree)
 | elem node children = parent
 | otherwise = findParent node tree

findBrothers :: Tree -> Int -> [Int]
findBrothers tree node = filter (/= node) (snd (head brothers))
 where
    parent = findParent node tree
    brothers = [(p, s) | (p, s) <- tree, p == parent]

