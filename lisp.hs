data Expr = LInt Int
          | LSym String
          | SExpr [Expr]
          deriving (Eq)
        
instance Show Expr where
    show (LInt x) = show x
    show (LSym x) = x
    show (SExpr xs) = "(" ++ unwords (map show xs) ++ ")"

type Program = [Expr]

consExpr :: Expr -> Expr -> Expr
consExpr x (SExpr xs) = SExpr (x:xs)
consExpr x y = SExpr [x,y]

brackets = ['(',')','[',']'] :: [Char]

digits = ['0','1','2','3','4','5','6','7','8','9'] :: [Char]

isInteger :: String -> Bool
isInteger = foldr (\ x -> (&&) (x `elem` digits)) True

isSymbol :: String -> Bool
isSymbol xs = not (isInteger xs || and [x `elem` brackets | x <- xs])

padBrackets :: String -> String
padBrackets [] = []
padBrackets (x:xs) | x `elem` brackets = [' ',x,' '] ++ padBrackets xs
                   | otherwise = x : padBrackets xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

isWhitespace :: String -> Bool
isWhitespace xs = and [x `elem` [' ','\t','\n'] | x <- xs]

takeExpr :: Int -> [String] -> [String]
takeExpr 0 _ = []
takeExpr n [] = []
takeExpr n (x:xs) | x == "(" = "(" : takeExpr (n+1) xs
                  | x == ")" = ")" : takeExpr (n-1) xs
                  | otherwise = x : takeExpr n xs

dropExpr :: Int -> [String] -> [String]
dropExpr 0 xs = xs
dropExpr n [] = []
dropExpr n (x:xs) | x == "(" = dropExpr (n+1) xs
                  | x == ")" = dropExpr (n-1) xs
                  | otherwise = dropExpr n xs

parse :: [String] -> (Expr,[String])
parse [] = (SExpr [],[])
parse ["("] = error "Mismatched brackets 1"
parse [")"] = (SExpr [],[])
parse [x] | isInteger x = (LInt (read x),[])
          | isSymbol x  = (LSym x,[])
parse ["(",x,")"] = (SExpr [parse' [x]],[])
parse ("(":")":xs) = error "Mismatched brackets 3"
parse ("(":"(":xs) = (consExpr (consExpr (parse' (takeExpr 1 xs)) (parse' (dropExpr 1 xs))) (parse' (dropExpr 1 ("(":xs))), [])
parse ("(":x:xs) = (consExpr (consExpr (parse' [x]) (parse' (takeExpr 1 xs))) (parse' (dropExpr 1 xs)), [])
parse (")":xs) = (SExpr [], xs)
parse [x,")"] = parse [x]
parse (x:xs) | isInteger x = (consExpr (LInt (read x)) (parse' xs), [])
             | isSymbol x  = (consExpr (LSym x) (parse' xs), [])

parse' :: [String] -> Expr
parse' = fst . parse

parse2 :: [String] -> Expr
parse2 [] = SExpr []
parse2 ["("] = error "Mismatched brackets 1"
parse2 [")"] = SExpr []
parse2 [x] | isInteger x = LInt (read x)
          | isSymbol x  = LSym x
parse2 ["(",x,")"] = SExpr [parse2 [x]]
parse2 ("(":")":xs) = error "Mismatched brackets 2"
parse2 ("(":"(":xs) = consExpr (consExpr (parse2 (takeExpr 1 xs)) (parse2 (dropExpr 1 xs))) (parse2 (dropExpr 1 ("(":xs)))
parse2 ("(":x:xs) = consExpr (consExpr (parse2 [x]) (parse2 (takeExpr 1 xs))) (parse2 (dropExpr 1 xs))
parse2 (")":xs) = SExpr []
parse2 [x,")"] = parse2 [x]
parse2 (x:xs) | isInteger x = consExpr (LInt (read x)) (parse2 xs)
              | isSymbol x  = consExpr (LSym x) (parse2 xs)

unpack :: Expr -> Expr
unpack (SExpr [SExpr xs]) = unpack (SExpr xs)
unpack x = x

mismatchedBrackets :: Int -> [String] -> [String]
mismatchedBrackets n _ | n < 0 = error "too many close brackets"
mismatchedBrackets n [] | n > 0 = error "too many open brackets"
                        | otherwise = []
mismatchedBrackets n (x:xs) | x == "(" = x : mismatchedBrackets (n+1) xs
                            | x == ")" = x : mismatchedBrackets (n-1) xs
                            | otherwise = x : mismatchedBrackets n xs
                    
squareToRound :: String -> String
squareToRound "[" = "("
squareToRound "]" = ")"
squareToRound x = x

-- eval :: Expr -> Expr

niceParse :: String -> Expr
niceParse = unpack . parse2 . (++[")"]) . ("(":) . mismatchedBrackets 0 . map squareToRound . words . padBrackets

test :: String -> ([String],[String])
test = (\y -> (takeExpr 1 y, dropExpr 1 y)) . words . padBrackets

main :: IO ()
main = do
    putStr ">>"
    x <- getLine
    if x == "q"
    then putStr ""
    else do
        case x of
          'r':' ':x -> readFile x >>= print . niceParse
          _ -> (print . niceParse) x
        main