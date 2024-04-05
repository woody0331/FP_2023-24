main :: IO()
main = do 
    print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
    print $ (repeater "Ivalin") 3 " 9MI0700224 " == "Ivalin 9MI0700224 Ivalin 9MI0700224 Ivalin" --my test

repeater :: String -> (Int -> String -> String)
repeater str = (\count glue -> concat (replicate (count - 1) (str ++ glue)) ++ str)