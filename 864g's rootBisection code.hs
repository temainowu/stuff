rootBisection :: (Eq a, Fractional a, Ord a) => a -> a -> a -> (a -> a) -> Int -> a
rootBisection tol a b f 0 = 0
rootBisection tol a b f n = do
    let x = (a+b) / 2
    if abs (f x) < tol
        then x
        else do
            if f x * f a < 0
                then rootBisection tol a x f (n-1)
                else rootBisection tol x b f (n-1)


rootBisection' :: (Eq a, Fractional a, Ord a) => a -> a -> a -> (a -> a) -> Int -> a
rootBisection' tol a b f 0 = 0
rootBisection' tol a b f n | abs (f x) < tol = x
                           | f x * f a < 0   = rootBisection' tol a x f (n-1)
                           | otherwise       = rootBisection' tol x b f (n-1)
                    where x = (a+b) / 2

main :: IO ()
main = print (rootBisection (1/10^5) 1 2 (\x -> x^2 - 2) 100)