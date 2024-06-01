import Data.List
main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False
    print $ isGraceful t3 == True --my test

data NTree a = Nil | Node a [NTree a] 
 deriving Show

t1 :: NTree Int
t1 = Node 1 [Node 3 [], Node 5 [], Node 7 [], Node 9 []]

t2 :: NTree Int
t2 = Node 7 [Node 9 [Node 5 [], Node 2 []]]

t3 :: NTree Int
t3 = Node 121 [Node 123 [], Node 533 [], Node 327 [], Node 219 []]

isGraceful :: (Integral a) => NTree a -> Bool
isGraceful Nil = True 
isGraceful (Node _ children) = all even differences && all isGraceful children
 where
    parentValue = head [x | Node x _ <- children] 
    childValues = [x | Node x _ <- children] 
    differences = map (\x -> abs (x - parentValue)) childValues