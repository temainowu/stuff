import Prelude hiding (Num, Word)

data Root = Lex (String, Def, Class) | Det Det
    deriving(Show, Eq)

data Det = Pro | Dist Bool
    deriving(Show, Eq)

data Class = Adj | Noun
    deriving(Show, Eq)

data Case = Nom | Acc | Gen | Dat
    deriving(Show, Eq)

data Gen = M | N | F
    deriving(Show, Eq)

data Num = S | P
    deriving(Show, Eq)

data Def = Strong | Weak
    deriving(Show, Eq)

newtype Word = W (Root, Case, Gen, Num)
    deriving (Show, Eq)

-- accusative → nominative/neuter ∨ plural
-- neuter → masculine/genitive ∨ dative
-- dative → genitive/feminine ∧ singular
-- singular → plural/accusative ∧ feminine
-- {masculine, neuter} → f eminine/plural ∧ (genitive ∨ dative)

inflect :: Word -> String
inflect (W (Det x, Nom, _, P)) = inflect (W (Det x, Nom, N, S)) ++ ":"
inflect (W (Det Pro, x, y, z)) = "hi" ++ inflect (W (Lex ("", Strong, Adj), x, y, z))
inflect (W (Det (Dist True), x, y, z)) = "þa" ++ inflect (W (Lex ("", Strong, Adj), x, y, z))
inflect (W (Det (Dist False), x, y, z)) = "þis" ++ inflect (W (Lex ("", Strong, Adj), x, y, z))
inflect (W (x, Acc, N, y)) = inflect (W (x, Nom, N, y))
inflect (W (x, Acc, y, P)) = inflect (W (x, Nom, y, P))
inflect (W (x, Gen, N, y)) = inflect (W (x, Gen, M, y))
inflect (W (x, Dat, N, y)) = inflect (W (x, Dat, M, y))
inflect (W (x, Dat, F, S)) = inflect (W (x, Gen, F, S))
inflect (W (x, Acc, F, S)) = inflect (W (x, Acc, F, P))
inflect (W (Lex (x, Weak, _), Gen, F, P)) = x ++ "ena"
inflect (W (Lex (x, Strong, Noun), Gen, F, P)) = x ++ "a"
inflect (W (Lex (x, Strong, Adj), Gen, F, P)) = x ++ "ra"
inflect (W (x, Gen, _, P)) = inflect (W (x, Gen, F, P))
inflect (W (Lex (x, _, _), Dat, F, P)) = x ++ "um"
inflect (W (x, Dat, _, P)) = inflect (W (x, Dat, F, P))
inflect (W (Lex (x, Weak, _), Nom, F, P)) = x ++ "an"
inflect (W (Lex (x, Strong, _), Nom, F, P)) = x ++ "e"
inflect (W (Lex (x, Weak, y), Nom, _, P)) = inflect (W (Lex (x, Weak, y), Nom, F, P))
inflect (W (Lex (x, Weak, _), Nom, M, S)) = x ++ "a"
inflect (W (Lex (x, Weak, _), Nom, _, S)) = x ++ "e"
inflect (W (Lex (x, Weak, _), _, _, S)) = x ++ "an"
inflect (W (Lex (x, Strong, _), Nom, M, S)) = x
inflect (W (Lex (x, Strong, _), Nom, N, S)) = x
inflect (W (Lex (x, Strong, Noun), Acc, M, S)) = x
inflect (W (Lex (x, Strong, _), Gen, M, S)) = x ++ "es"
inflect (W (Lex (x, Strong, Noun), Dat, M, S)) = x ++ "e"
inflect (W (Lex (x, Strong, Noun), Nom, M, P)) = x ++ "as"
inflect (W (Lex (x, Strong, _), Nom, F, S)) | heavy x = x
                                            | otherwise = x ++ "u"
inflect (W (Lex (x, Strong, _), Nom, N, P)) | heavy x = x
                                            | otherwise = x ++ "u"
inflect (W (Lex (x, Strong, Noun), Gen, F, S)) = x ++ "e"
inflect (W (Lex (x, Strong, Adj), Nom, M, P)) = x ++ "e"
inflect (W (Lex (x, Strong, Adj), Acc, M, S)) = x ++ "ne"
inflect (W (Lex (x, Strong, Adj), Dat, M, S)) = inflect (W (Lex (x, Strong, Adj), Dat, M, P))
inflect (W (Lex (x, Strong, Adj), Gen, F, S)) = x ++ "re"

heavy :: String -> Bool
heavy "" = False
heavy [_] = False
heavy x | consonant(last (init x)) && consonant(last x) = True
heavy _ = False

consonant :: Char -> Bool
consonant 'a' = False
consonant 'e' = False
consonant 'i' = False
consonant 'o' = False
consonant 'u' = False
consonant 'y' = False
consonant _ = True


makeword :: Either (String, Def, Class, Gen) Det -> [Word]
makeword (Left (x, d, c, g)) = [W (Lex (x, d, c), k, g, n) | n <- [S, P], k <- [Nom, Acc, Gen, Dat]]
makeword (Right x) = [W (Det x, k, g, n) | g <- [M, N, F], n <- [S, P], k <- [Nom, Acc, Gen, Dat]]

{-
+Q hw
+D þ
+D+Nom-P+E s
+D+P þi
+Pro hi

+Nom+E e:
+N t

+F u

+P s


-}
