

choose :: Int -> Int -> Int
choose n m = fact n `div` (fact m * fact (n-m))

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

