import Language.Haskell.TH (safe)
import Distribution.Simple.Utils (xargs)
data R = C | P | S
    deriving Show
data G = M | F
data A = Y | O
newtype Relation = R (Maybe G,R)
newtype K = K ([Relation],Maybe (A,Int))

instance Show A where
    show Y = "y"
    show O = "o"

instance Show G where
    show M = "m"
    show F = "f"

instance Show Relation where
    show (R (Nothing,r)) = show r
    show (R (Just g,r)) = show g ++ show r

instance Show K where
    show (K ([],Nothing)) = ""
    show (K (r:xs,Nothing)) = show (K (xs,Nothing)) ++ show r
    show (K (xs,Just (a,0))) = show (K (xs,Nothing)) ++ show a
    show (K (r:xs,Just (a,n))) = show (K (xs,Just (a,n-1))) ++ show r

instance Read K where
    read x = map read' (f x)
    read' :: [String] -> K
    read' ()

toHead :: (a -> a) -> [a] -> [a]
toHead f (x:xs) = f x : xs

f :: String -> [String]
f (x:xs) | isUpper x = x:f xs 
         | otherwise = toHead (x:) f xs

gen :: K -> Int
gen (K ([],_)) = 0
gen (K (R (_,S):xs,_)) = gen (K (xs,Nothing))
gen (K (R (_,P):xs,_)) = gen (K (xs,Nothing)) + 1
gen (K (R (_,C):xs,_)) = gen (K (xs,Nothing)) - 1

check :: K -> Bool
check (K (xs,Just (_,n))) | n < 0 || n >= length xs = False

ego :: K
ego = K ([],Nothing)

addRel :: R -> K -> K
addRel r (K (xs,a)) = K ((R (xs++[])),a) 

zh :: K -> String
zh (K ([R (Nothing,P),R (Just M,C)],Just (O,0))) = "哥哥"