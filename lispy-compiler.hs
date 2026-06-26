
data Composition = FA | PA | LC
  deriving (Eq)

-- Function Application
-- Predicate Abstraction
-- List Construction ??? <- I think????

data Lexeme = Open Composition | Close Composition | I Int | V String
  deriving (Eq)

data Tree = Branch Composition Tree Tree
          | Integer Int
          | Var String
          | Empty
          | Error [String]

data Type = INT | FUN Type Type | PAR Type Type | ErrorType
  deriving (Eq)

newtype Library = Lib [(String,(Type,Tree))]

std :: Library
std = Lib [("+", (FUN INT (FUN INT INT), Var "+"))]

instance Show Tree where
    show (Branch FA a (Branch FA b c)) = "(" ++ show a ++ " " ++ init (tail (show (Branch FA b c))) ++ ")"
    show (Branch FA a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Branch PA a (Branch PA b c)) = "<" ++ show a ++ " " ++ init (tail (show (Branch PA b c))) ++ ">"
    show (Branch PA a b) = "<" ++ show a ++ " " ++ show b ++ ">"
    show (Branch LC a (Branch LC b c)) = "[" ++ show a ++ " " ++ init (tail (show (Branch LC b c))) ++ "]"
    show (Branch LC a b) = "[" ++ show a ++ " " ++ show b ++ "]"
    show (Integer x) = show x
    show (Var x) = x
    show Empty = "()"
    show (Error x) = show x

toList :: Tree -> [String]
toList (Branch _ x Empty) = toList x
toList (Branch _ Empty y) = toList y
toList (Branch FA x y) = (("__" ++ head (toList x))
                     : map ("| " ++) (tail (toList x))) ++
                     (("L_" ++ head (toList y))
                     : map ("  " ++) (tail (toList y)))
toList (Branch PA x y) = (("O_" ++ head (toList x))
                     : map ("| " ++) (tail (toList x))) ++
                     (("L_" ++ head (toList y))
                     : map ("  " ++) (tail (toList y)))
toList (Branch LC x y) = (("*_" ++ head (toList x))
                     : map ("| " ++) (tail (toList x))) ++
                     (("L_" ++ head (toList y))
                     : map ("  " ++) (tail (toList y)))
toList (Var x) = [x]
toList (Integer x) = [show x]
toList Empty = []

brackets = ['(',')','<','>','[',']'] :: [Char]

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

tokenise :: String -> [String]
tokenise [] = []
tokenise xs = (filter (not . null) . (\(a,b) -> a : tokenise (safeTail b))) (break (isWhitespace . (: [])) xs)

lexer :: [String] -> [Lexeme]
lexer [] = []
lexer (x:xs) | x == "(" = Open FA : lexer xs
             | x == ")" = Close FA : lexer xs
             | x == "<" = Open PA : lexer xs
             | x == ">" = Close PA : lexer xs
             | x == "[" = Open LC : lexer xs
             | x == "]" = Close LC : lexer xs
             | isInteger x = I (read x) : lexer xs
             | isSymbol x = V x : lexer xs

parse :: [Lexeme] -> (Tree,[Lexeme])
parse [] = (Empty,[])
parse [Open _] = (Empty,[])
parse [Close _] = (Empty,[])
parse [V x] = (Var x,[])
parse [I x] = (Integer x,[])
parse (Open FA:xs) = (Branch FA (fst (parse xs)) (fst (parse (snd (parse xs)))), snd (parse (snd (parse xs))))
parse (Open PA:xs) = (Branch PA (fst (parse xs)) (fst (parse (snd (parse xs)))), snd (parse (snd (parse xs))))
parse (Open LC:xs) = (Branch LC (fst (parse xs)) (fst (parse (snd (parse xs)))), snd (parse (snd (parse xs))))
parse (Close _:xs) = (Empty, xs)
parse (V x:xs) = (Var x, xs)

mismatchedBrackets :: (Tree,[Lexeme]) -> Tree
mismatchedBrackets (x,ys) | null ys = x
                          | otherwise = Error ["ERROR: Mismatched brackets"]

niceParse :: String -> Tree
niceParse = mismatchedBrackets . parse . lexer . tokenise . padBrackets

takes :: Type -> Type -> Bool
takes (FUN x _) y = x == y
takes _ _ = False

safeLookup :: String -> Library -> (Type, Tree)
safeLookup x (Lib es) = case lookup x es of
                    Nothing -> (ErrorType, Error ["Definition of "++ x ++" does not c-command use"])
                    Just y -> y

fapply :: Tree -> Tree -> Tree
fapply a b = a

eval :: Library -> Tree -> Tree
eval es (Branch _ x Empty) = eval es x
eval es (Branch _ Empty y) = eval es y
eval _ (Branch _ (Error x) (Error y)) = Error (x ++ y)
eval _ (Branch _ (Error x) y) = Error x
eval _ (Branch _ x (Error y)) = Error y
eval es (Branch FA (Var x) (Var y)) = if takes (fst (safeLookup x es)) (fst (safeLookup y es))
                               then fapply (snd (safeLookup x es)) (snd (safeLookup y es))
                               else Error ["Type mismatch"]
eval (Lib es) (Integer x) = Integer x

main :: IO ()
main = do
    putStr ">>"
    x <- getLine
    if x == "q"
    then putStr ""
    else do
        case x of
          'r':' ':x -> readFile x >>= {-mapM_ putStrLn . toList . niceParse-} print . niceParse
          _ -> (mapM_ putStrLn . toList . niceParse) x
        main

{-
binary composition:
(expr expr) - function application
<var expr> - predicate abstraction
[expr expr] - list
{ch} - string
\var expr/ - raised the c-command potential of a var over an expr, to allow for recursive definitions.

n-ary composition patterns:
(expr expr ... expr expr) -> (expr (expr (... (expr expr))))
<var  var  ... var  expr> -> <var  <var  <... <var  expr>>>>
[expr expr ... expr expr] -> [expr [expr [... [expr expr]]]]

Examples:
lambda:
(\x -> M) N ~> (N <x M>)

function definition:
let f := M; N ~> (M <f N>)

case marking:
f :: a -> b -> c -> X - arguments are marked syntactically.
(k3 k2 k1 f)          - arguments are marked by case:

((k3 k2 k1 f) <f'     - defines f' as a case marked f (f' :: X)
(N <k1 f'>)           - "N <k1" marks N as the first case of f'
>) 
-}