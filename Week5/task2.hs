main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364
    print $ getPalindromes 141141 == 141144 --my test

getPalindromes :: Int -> Int
getPalindromes n = minimum (palindromeDivisors n) + maximum (palindromeDivisors n)

palindromeDivisors :: Int -> [Int]
palindromeDivisors n = [d | d <- [2 .. n], mod n d == 0, isPalindrome d]

isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)