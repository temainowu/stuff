
data Tree = B [Tree] | N String | Error

fromMaybe :: Maybe [String] -> Tree
fromMaybe Nothing = Error
fromMaybe (Just x) = aux1 0 x

aux2 :: Int -> String -> (String,String)
aux2 0 x = (x,[])
aux2 n x = (\ u (a,b) -> (u ++ a, b)) (takeWhile (/= ']') x) (aux2 (n-1) (dropWhile (/= ']') x))

func :: String -> Tree
func x = fromMaybe (aux 0 x)
    where
        aux :: Int -> String -> Maybe [String]
        aux 0 [] = Just []
        aux n [] = Nothing
        aux n ('[':xs) = maybecons "[" (aux (n + 1) xs)
        aux 0 (']':xs) = Nothing
        aux n (']':xs) = maybecons "]" (aux (n - 1) xs)
        aux n xs = maybecons p1 (aux n p2)
                    where
                        p1 = takeWhile (`notElem` ['[',']']) xs
                        p2 = dropWhile (`notElem` ['[',']']) xs

        maybecons :: String -> Maybe [String] -> Maybe [String]
        maybecons x Nothing = Nothing
        maybecons x (Just xs) = Just (x : xs)

func1 :: [String] -> Tree
func1 [] = 
func1 ("[" : xs) = 