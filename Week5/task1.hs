import Data.Char(intToDigit)
main :: IO()
main = do
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 200 1 == [7,17,37,47,67,71,73,79,97,107,127,137,157,167,173,179,197] --my test

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 200 1 == [7,17,37,47,67,71,73,79,97,107,127,137,157,167,173,179,197] --my test

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC start end = [ x | x <- [min start end .. max start end], isPrime x && elem (intToDigit 7) (show x) ]

getPrimesHOF :: Int -> Int -> [Int]
getPrimesHOF start end = filter (\ x -> isPrime x && elem (intToDigit 7) (show x)) [min start end .. max start end]

isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n]