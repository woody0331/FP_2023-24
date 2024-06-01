main::IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))
    print $ convert tree2 == Node 57 (Node 76 (Node 88 Nil Nil) (Node 66 Nil Nil)) (Node 43 Nil (Node 22 Nil Nil)) --my test

data BTree = Nil | Node Int BTree BTree
 deriving (Show, Eq)

tree :: BTree
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

tree2 :: BTree
tree2 = Node 14 (Node 10 (Node 12 Nil Nil) (Node 9 Nil Nil)) (Node 21 Nil (Node 22 Nil Nil))

convert :: BTree -> BTree
convert tree = fst $ helper tree 0
 where
    helper :: BTree -> Int -> (BTree,Int)
    helper Nil value = (Nil,value)
    helper (Node v l r) value = (Node newValue newLeft newRight, finalSum)
     where
        (newRight, rightSum) = helper r value
        newValue = v + rightSum
        (newLeft, finalSum) = helper l newValue