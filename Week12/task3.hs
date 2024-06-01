main::IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

    print $ levelSum myBTree 1 == 31 --my test
    print $ cone myBTree == True --my test

data BTree = Empty | Node Int BTree BTree
 deriving (Show, Eq)

numberBTree :: BTree
numberBTree = Node 10 (Node 5 (Node 1 Empty Empty) (Node 9 Empty Empty)) (Node 6 (Node 8 Empty Empty) (Node 7 Empty Empty))

myBTree :: BTree
myBTree = Node 14 (Node 10 (Node 22 Empty Empty) (Node 19 Empty Empty)) (Node 21 Empty (Node 22 Empty Empty))

levelSum :: BTree -> Int -> Int
levelSum Empty _ = 0
levelSum (Node value left right) 0 = value
levelSum (Node _ left right) k = levelSum left (k - 1) + levelSum right (k - 1)

cone :: BTree -> Bool
cone tree = checkCone tree 0 (-1)

checkCone :: BTree -> Int -> Int -> Bool
checkCone tree level prevSum
 | currentSum == 0 = True  
 | prevSum >= 0 && currentSum <= prevSum = False
 | otherwise = checkCone tree (level + 1) currentSum
 where 
    currentSum = levelSum tree level