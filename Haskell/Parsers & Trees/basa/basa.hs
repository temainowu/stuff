import System.IO
import Data.Char
import Data.List

data Type =
    Num | Str | Prop | Type | List Type | Unknown |
    Index Int | Error |
    Var Type Int | Type :-> Type | Type :<- Type | Or Type Type | And Type Type | Bind Type Type
   deriving Eq



data Tree = Node String | Branch Tree Tree | E String

data TypeTree = N String Type | B TypeTree TypeTree Type

instance Show Tree where
    show :: Tree -> String
    show (E x) = "Error: " ++ x
    show (Node x) = x
    show (Branch x y) = '(' : show x ++ " ^ " ++ show y ++ ")"

typeOfTree :: TypeTree -> Type
typeOfTree (N _ t) = t
typeOfTree (B _ _ t) = t

labelOfTree :: TypeTree -> [String]
labelOfTree (N s _) = [s]
labelOfTree (B x y _) = labelOfTree x ++ labelOfTree y

instance Show Type where
    show Num = "ℤ"
    show Str = "S"
    show Prop = "2"
    show Type = "T"
    show (List t) = '[' : show t ++ "]"
    show (Index n) = show n
    show Unknown = "??"
    show Error = "!!"
    show (Var t n) = show t ++ "_" ++ show n
    show (t0 :-> t1) = show t0 ++ "→" ++ show t1
    show (t0 :<- t1) = show t0 ++ "←" ++ show t1
    show (Or t0 t1) = show t0 ++ "+" ++ show t1
    show (And t0 t1) = show t0 ++ "×" ++ show t1
    show (Bind t0 t1) = show t0 ++ "|>" ++ show t1

instance Show TypeTree where
    show (N x t) = '{' : x ++ " :: " ++ show t ++ "}"
    show (B x y t) = '{' : '(' : (show x ++ "^" ++ show y) ++ ") :: " ++ show t ++ "}"

space :: Char -> Bool
space '\n' = True
space ' ' = True
space _ = False

openB :: Char -> Maybe Char
openB '[' = Just ']'
openB '(' = Just ')'
openB '{' = Just '}'
openB _ = Nothing

isBrac :: Char -> Bool
isBrac '[' = True
isBrac ']' = True
isBrac '(' = True
isBrac ')' = True
isBrac '{' = True
isBrac '}' = True
isBrac _ = False

isOpen :: Char -> Bool
isOpen '[' = True
isOpen '(' = True
isOpen '{' = True
isOpen _ = False

isClos :: Char -> Bool
isClos ']' = True
isClos ')' = True
isClos '}' = True
isClos _ = False

findClose :: Int -> Int -> Char -> [String] -> Maybe Int
findClose d p b [] = Nothing
findClose 0 p b ([x]:xs) | Just x == openB b = Just p
                         | isOpen x = findClose 1 (p+1) b xs
                         | isClos x = Nothing
                         | otherwise = findClose 0 (p+1) b xs
findClose d p b ([x]:xs) | isOpen x = findClose (d+1) (p+1) b xs
                         | isClos x = findClose (d-1) (p+1) b xs
                         | otherwise = findClose d (p+1) b xs

parse :: String -> [String]
parse [] = []
parse [x] | space x   = []
          | otherwise = [[x]]
parse (x : y : xs) | space x   = parse (y : xs)
                   | space y   = [x] : parse xs
                   | isBrac y  = [x] : [y] : parse xs
                   | isBrac x  = [x] : parse (y : xs)
                   | otherwise = (x : head (parse (y : xs))) : tail (parse (y : xs))

readTree :: [String] -> Tree
readTree [] = E "Empty Input"
readTree [[]] = Node ""
readTree [[x]] | isBrac x = E "Mismatched Bracket at End of File"
               | otherwise = Node [x]
readTree ([x]:xs) | isBrac x = case findClose 0 0 x xs of
                        Nothing -> E "Mismatched Brackets"
                        Just n  -> if n+1 == length xs
                                   then readTree (take n xs)
                                   else Branch (readTree (take n xs)) (readTree (drop (n+1) xs))
                  | otherwise = Branch (Node [x]) (readTree xs)
readTree (x:xs) = Branch (Node x) (readTree xs)

typeOf :: String -> Type
typeOf "all" = Bind Prop Prop
typeOf "some" = Bind Prop Prop
typeOf "sum" = List Num :-> Bind Num Num
typeOf "prod" = Bind Num Num
typeOf "Num" = Type
typeOf "T" = Prop
typeOf "F" = Prop
typeOf "[]" = List Unknown
typeOf ":" = (Unknown :-> List Unknown) :<- List Unknown
-- typeOf ('[' : x) | last x == ']' = List (typeOf (takeWhile (/= ':') x))
typeOf ('_' : x) | all isNumber x = Index (read x)
                 | otherwise = Error
typeOf ('\"' : x) | last x == '\"' = Str
                  | otherwise = Error
typeOf x | all isNumber x = Num
         | otherwise = Error

mergeType :: Type -> Type -> Maybe Type
mergeType (Bind x y) (Var z _) | x == z = Just y
                               | otherwise = Nothing
mergeType (Bind x y) z | x == z = Just y
                       | otherwise = Nothing
mergeType (x :-> y) (Var z _) | x == z = Just y
                              | otherwise = Nothing
mergeType (x :-> y) z | x == z = Just y
                | otherwise = Nothing
mergeType  (Var z _) (x :<- y) | y == z = Just y
                               | otherwise = Nothing
mergeType z (x :<- y) | y == z = Just y
                      | otherwise = Nothing


readbasa :: Tree -> TypeTree
readbasa (E x) = N x Error
readbasa (Node x) = case typeOf x of
                    Error -> N ("Parsing error on: \"" ++ x ++ "\"") Error
                    t     -> N x t
readbasa (Branch x y) = case mergeType xt yt of
                            Nothing -> N "Type Mismatch" Error
                            Just t -> B (readbasa x) (readbasa y) t
    where
        xt = typeOfTree (readbasa x)
        yt = typeOfTree (readbasa y)

main :: IO ()
main = openFile "main.txt" ReadMode >>= hGetContents >>= print . readbasa . readTree . parse
