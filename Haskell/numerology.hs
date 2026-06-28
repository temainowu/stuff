import Data.List

fsqrt :: Int -> Int
fsqrt = floor . sqrt . fromIntegral

countdiv :: Int -> Int -> Int
countdiv m n | n * div m n == m = 1 + countdiv (m `div` n) n
             | otherwise        = 0

f :: Int -> [(Int, Int)]
f x = [ (p, countdiv x p) |
        p <- [2 .. div x 2],
            and [ n * div p n /= p | n <- [2 .. fsqrt p]],
            (x `mod` p) == 0]

smoosh :: [(Int, Int)] -> [Int]
smoosh [] = []
smoosh ((p,0) : xs) = smoosh xs
smoosh ((p,n) : xs) = p : smoosh ((p, n-1) : xs)

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x : xs) | x `elem` xs = dedup xs
               | otherwise   = x : dedup xs

k :: Int -> Int -- A008480
k = length . dedup . permutations . smoosh . f

---

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

j :: Int -> Int -- A112624
j = product . map (fact . snd) . f

--- 

bigOmega :: Int -> Int -- A144494
bigOmega = sum . map snd . f

i :: Int -> Int -- A130675
i = fact . bigOmega

---

omega :: Int -> Int -- A087624
omega = length . f

l :: Int -> Int
l = fact . omega

---

seqs :: [(Int -> Int, Int -> Int)]
seqs = [(const 1, i), (const 1, j), (const 1, k), (const 1, l), (i, j), (i, k), (i, l), (j, k), (j, l), (k, l)]

thing :: [(Int -> Int, Int -> Int)] -> [[Int]]
thing = map (\ (f, g) -> zipWith (*) (map f [0 .. 36]) (map g [0 .. 36]))

test :: Bool
test = thing seqs == dedup (thing seqs)

test' :: [(Int -> Int, Int -> Int)] -> Bool
test' s = thing s == dedup (thing s)

-- test' [(const 1, i), (j, k)]

-- i = j * k 
-- :)
-- was wondering if that was true
-- but im terrible at combinatorial thinking

---

d :: Int -> (Int, Int)
d x = (k x, j x)

---

data Tree = N Int | B Tree Tree

instance Eq Tree where 
    (==) :: Tree -> Tree -> Bool
    (==) (B _ _) (N _) = False
    (==) (N _) (B _ _) = False
    (==) (N x) (N y) = x == y 
    (==) (B xl xr) (B yl yr) = (xl == yl && xr == yr) || (xl == yr && xr == yl)

toList :: Tree -> [String]
toList (B x y) = (("__" ++ head (toList x))
                     : map ("| " ++) (tail (toList x))) ++
                     (("L_" ++ head (toList y))
                     : map ("  " ++) (tail (toList y)))
toList (N x) = [show x]

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where 
    show :: NoQuotes -> String
    show (NoQuotes str) = str

newtype PrettyList a = PrettyList [a]

instance Show a => Show (PrettyList a) where
    show :: Show a => PrettyList a -> String
    show (PrettyList []) = []
    show (PrettyList (x:xs)) = show x ++ ('\n' : show (PrettyList xs))

instance Show Tree where
    show :: Tree -> String
    show = show . PrettyList . map NoQuotes . ("\n" :) . toList

length' :: PrettyList a -> Int 
length' (PrettyList xs) = length xs

tlength :: Tree -> Int
tlength (N _) = 1
tlength (B x y) = tlength x + tlength y

allCuts :: [a] -> [([a],[a])]
allCuts [] = []
allCuts [x] = []
allCuts (x : xs) = ([x],xs) : allCuts xs

removeNth :: Int -> [a] -> [a]
removeNth _ [] = []
removeNth 0 (x : xs) = xs 
removeNth n (x : xs) = x : removeNth (n - 1) xs

auxtperm :: [Int] -> [(Tree, [Int])]
auxtperm [] = []
auxtperm xs = [ (N (xs !! i), removeNth i xs) | i <- [0 .. length xs - 1]]

auxtperm' :: [Int] -> [(Tree, [Int])]
auxtperm' xs = [ (B (N (xs !! i)) (N (xs !! j)), removeNth j (removeNth i xs)) | i <- [0 .. length xs - 2], j <- [i + 1, length xs - 1]]

tpermRec :: (Tree, [Int]) -> [Tree]
tpermRec (t, []) = [t]
tpermRec (t, [x]) = [B t (N x)] 
tpermRec (t, xs) = dedup (concat [ tpermRec (B t (N (xs !! i)), removeNth i xs) | i <- [0 .. length xs - 1]]
                   ++ concatMap (\ (t', xs') -> map (B t) (tpermRec (t', xs'))) (auxtperm xs))

tpermutations :: [Int] -> [Tree]
tpermutations [] = []
tpermutations [x] = [N x]
tpermutations xs = dedup (concatMap tpermRec (auxtperm xs))

prettyTPerm :: [Int] -> PrettyList Tree
prettyTPerm = PrettyList . tpermutations

tpermCheck :: [Int] -> Bool
tpermCheck xs = all ((length xs ==) . tlength) (tpermutations xs)

h :: Int -> PrettyList Tree
h = prettyTPerm . smoosh . f

numerolog :: Int -> Int
numerolog = length . tpermutations . smoosh . f

oooh :: [PrettyList Tree]
oooh = map (\ x -> prettyTPerm (replicate x 0)) [1 .. 6]

ooohLen :: [Int] 
ooohLen = map (\ x -> length (tpermutations (replicate x 0))) [1 ..]
-- should be A000055:
-- 1,1,1,2,3,6,11,23,...
-- but actually gives:
-- 1,1,1,2,3,6,11,22,...
-- ???

main :: IO ()
main = print (map h [5,6,12,16,30])

