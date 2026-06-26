data F u v = H v | T (u -> F u v)

kase :: Int -> a -> F a b -> F a b
kase 0 x (T f) = f x
kase n x (T f)  = T (\a -> kase (n-1) x (f a))

f :: [a] -> a -> Bool
f xs = `elem` xs

a :: [a] -> a -> Bool -> (a -> Bool) -> Bool
a qs x p = all [x q <= p q | q <- qs]

e :: [a] -> a -> Bool -> (a -> Bool) -> Bool
e qs x p = or []