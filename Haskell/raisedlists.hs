raise :: ([a] -> [a]) -> (a -> [a]) -> a -> [a]
raise g f x = x : (g . tail . f) x

emptyList :: a -> [a]
emptyList = (: [])

raiseList :: [a] -> a -> [a]
raiseList xs = (: xs)

dropList :: (Int -> [Int]) -> [Int]
dropList f = tail (f 0)