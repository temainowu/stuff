import Data.Char
import Data.List

data Pos = A Int | B Int | C Int | D Int
    deriving (Eq)

instance Show Pos where
    show (A x) = 'A' : show x
    show (B x) = 'B' : show x
    show (C x) = 'C' : show x
    show (D x) = 'D' : show x

newtype Display = Display [[Maybe Bool]]

instance Show Display where
    show (Display [[]]) = "\n"
    show (Display ([]:xs)) = '\n' : show (Display xs)
    show (Display ((Nothing:xs):xss)) = "🕵️ " ++ show (Display (xs:xss))
    show (Display ((Just True:xs):xss)) = "🙆 " ++ show (Display (xs:xss))
    show (Display ((Just False:xs):xss)) = "🙅 " ++ show (Display (xs:xss))

board :: [[Maybe Bool]]
board = [[Nothing, Nothing, Nothing, Nothing],
         [Nothing, Nothing, Nothing, Nothing],
         [Nothing, Nothing, Nothing, Nothing],
         [Nothing, Nothing, Nothing, Nothing],
         [Nothing, Nothing, Nothing, Nothing]]

-- Logic Functions

cat :: x -> [[x]] -> [[x]]
cat x [[]] = [[x]]
cat x (xs:xss) = (x:xs):xss

breakList :: String -> Char -> [String]
breakList [] c = []
breakList xs c = takeWhile (/= c) xs : breakList (drop 1 (dropWhile (/= c) xs)) c

putStrLns :: [String] -> IO ()
putStrLns = mapM_ putStrLn

isInt :: Char -> Bool
isInt '0' = True
isInt '1' = True
isInt '2' = True
isInt '3' = True
isInt '4' = True
isInt '5' = True
isInt '6' = True
isInt '7' = True
isInt '8' = True
isInt '9' = True
isInt _ = False

removeWhiteSpace :: String -> String
removeWhiteSpace [] = []
removeWhiteSpace (' ':xs) =     removeWhiteSpace xs
removeWhiteSpace ( x :xs) = x : removeWhiteSpace xs

-- [Pos]

possPos :: [Pos] -> [Pos]
possPos [] = []
possPos (x:xs) | 0 <= yc x && yc x <= 4 = x : possPos xs
               | otherwise              =     possPos xs

neighbours :: Pos -> [Pos]
neighbours (A x) = possPos [A (x-1), A (x+1), B x, B (x-1), B (x+1)]
neighbours (D x) = possPos [D (x-1), D (x+1), C x, C (x-1), C (x+1)]
neighbours (B x) = possPos [B (x-1), B (x+1), A x, A (x-1), A (x+1), C x, C (x-1), C (x+1)]
neighbours (C x) = possPos [C (x-1), C (x+1), B x, B (x-1), B (x+1), D x, D (x-1), D (x+1)]

edge :: [Pos]
edge = [A 1, A 2, A 3, A 4, A 5, B 1, B 5, C 1, C 5, D 1, D 2, D 3, D 4, D 5]

row :: Int -> [Pos]
row x = possPos [A x, B x, C x, D x]

colA :: [Pos]
colA = [A 1, A 2, A 3, A 4, A 5]

colB :: [Pos]
colB = [B 1, B 2, B 3, B 4, B 5]

colC :: [Pos]
colC = [C 1, C 2, C 3, C 4, C 5]

colD :: [Pos]
colD = [D 1, D 2, D 3, D 4, D 5]

dir :: Int -> Pos -> [Pos]
dir 3 (A x) = map A [x+1..5]
dir 3 (B x) = map B [x+1..5]
dir 3 (C x) = map C [x+1..5]
dir 3 (D x) = map D [x+1..5]
dir 1 (A x) = map A [1..x-1]
dir 1 (B x) = map B [1..x-1]
dir 1 (C x) = map C [1..x-1]
dir 1 (D x) = map D [1..x-1]
dir 0 (A x) = [B x, C x, D x]
dir 0 (B x) = [C x, D x]
dir 0 (C x) = [D x]
dir 2 (D x) = [A x, B x, C x]
dir 2 (C x) = [A x, B x]
dir 2 (B x) = [A x]

dir' :: Int -> Pos -> [Pos]
dir' 0 (A x) = [B x]
dir' 0 (B x) = [C x]
dir' 0 (C x) = [D x]
dir' 2 (B x) = [A x]
dir' 2 (C x) = [B x]
dir' 2 (D x) = [C x]
dir' 1 (A x) = possPos [A (x-1)]
dir' 1 (B x) = possPos [B (x-1)]
dir' 1 (C x) = possPos [C (x-1)]
dir' 1 (D x) = possPos [D (x-1)]
dir' 3 (A x) = possPos [A (x+1)]
dir' 3 (B x) = possPos [B (x+1)]
dir' 3 (C x) = possPos [C (x+1)]
dir' 3 (D x) = possPos [D (x+1)]

between :: Pos -> Pos -> [Pos]
between (A x) (A y) | x > y     = between (A y) (A x)
                    | x == y    = []
                    | otherwise = map A [x+1..y-1]
between (B x) (B y) | x > y     = between (B y) (B x)
                    | x == y    = []
                    | otherwise = map B [x+1..y-1]
between (C x) (C y) | x > y     = between (C y) (C x)
                    | x == y    = []
                    | otherwise = map C [x+1..y-1]
between (D x) (D y) | x > y     = between (D y) (D x)
                    | x == y    = []
                    | otherwise = map D [x+1..y-1]
between x y | yc x > yc y       = between y x
            | xc y - xc x <= 1  = []
            | yc x == yc y      = case (xc x, xc y) of
                                    (0, 2) -> [B c]
                                    (0, 3) -> [B c, C c]
                                    (1, 3) -> [C c]
            | otherwise         = []
            where c = yc x + 1

-- Rule logic

no :: [Pos] -> Bool -> [[Maybe Bool]] -> Bool
no xs s x = 0 == count s (map (getPos x) xs)

connected :: Bool -> [Pos] -> [[Maybe Bool]] -> Bool
connected t ps b = func [ fst x | x <- map (\p -> (p, b !! yc p !! xc p)) ps, snd x == Just t ]
    where func xs = and [ between x y == (between x y `intersect` xs) | x <- xs, y <- xs ]

count :: Bool -> [Maybe Bool] -> Int
count _ [] = 0
count y (Just x:xs) | x /= y = count y xs
                    | x == y = count y xs + 1

-- Board coördinate look-up

xc :: Pos -> Int
xc (A _) = 0
xc (B _) = 1
xc (C _) = 2
xc (D _) = 3

yc :: Pos -> Int
yc (A n) = n - 1
yc (B n) = n - 1
yc (C n) = n - 1
yc (D n) = n - 1

getPos :: [[Maybe Bool]] -> Pos -> Maybe Bool
getPos xs p = xs !! yc p !! xc p

updateBoard :: [[Maybe Bool]] -> Bool -> Pos -> [[Maybe Bool]]
updateBoard b x p = take (yc p) b ++ ((take (xc p) (b !! yc p) ++ (Just x : drop (xc p + 1) (b !! yc p))) : drop (yc p + 1) b)

-- Possibilities

possibilities :: [[Maybe Bool]] -> [[[Maybe Bool]]]
possibilities [[Just x]] = [[[Just x]]]
possibilities ([Just x]:xss) = map ([Just x] :) (possibilities xss)
possibilities ((Just x:xs):xss) = map (cat (Just x)) (possibilities (xs:xss))
possibilities [[Nothing]] = [[[Just True]], [[Just False]]]
possibilities ([Nothing]:xss) = map ([Just True] :) (possibilities xss) ++ map ([Just False] :) (possibilities xss)
possibilities ((Nothing:xs):xss) = map (cat (Just True)) (possibilities (xs:xss)) ++ map (cat (Just False)) (possibilities (xs:xss))

applyRules :: [[[Maybe Bool]]] -> [[[Maybe Bool]] -> Bool] -> [[Maybe Bool]]
applyRules ps [] = mutPoss ps
applyRules ps (f:rs) = applyRules [p | p <- ps, f p] rs

mutPoss :: [[[Maybe Bool]]] -> [[Maybe Bool]]
mutPoss [] = board
mutPoss [xs] = xs
mutPoss [[]:xs, []:ys] = [] : mutPoss [xs, ys]
mutPoss [(Nothing:xs):xss, (_:ys):yss] = cat Nothing (mutPoss [xs:xss, ys:yss])
mutPoss [(_:xs):xss, (Nothing:ys):yss] = cat Nothing (mutPoss [xs:xss, ys:yss])
mutPoss [(Just x:xs):xss, (Just y:ys):yss] | x == y    = cat (Just x) (mutPoss [xs:xss, ys:yss])
                                           | otherwise = cat Nothing (mutPoss [xs:xss, ys:yss])
mutPoss [[],[]] = []
mutPoss (x:y:xs) = mutPoss (mutPoss [x, y] : xs)

-- Parsing input

stb :: String -> Bool
stb ('i':'n':'n':'o':'c':'e':'n':'t':_) = True
stb ('c':'r':'i':'m':'i':'n':'a':'l':_) = False

readPoses :: String -> [Pos]
readPoses = f readPos
    where
        f :: (a -> a -> b) -> [a] -> [b]
        f g [] = []
        f g (x:y:xs) = g x y : f g xs

        readPos :: Char -> Char -> Pos
        readPos 'a' x = A (read [x])
        readPos 'b' x = B (read [x])
        readPos 'c' x = C (read [x])
        readPos 'd' x = D (read [x])

--

parseRule :: [String] -> [[Maybe Bool]] -> Bool
-- row 2 is the only row with exactly 3 innocents
-- Rule: only row 2 3 innocent
parseRule ("only":"row":x:xs) b =  all (\f -> f b) (map ((not .) . parseRule . (xs ++) . (:[]) . map toLower . removeWhiteSpace . concatMap show . row) ([ n | n <- [1..5], n /= read x])
                                   ++ [parseRule (xs ++ [removeWhiteSpace (map toLower (concatMap show (row (read x))))])])
-- Each row has more than 2 criminals
-- Rule: Each row more than 2 criminals
parseRule ("each":"row":xs) b = all ((\f -> f b) . parseRule . (xs ++) . (:[]) . map toLower . removeWhiteSpace . concatMap show . row) [1 .. 5]
-- Exactly two rows have 3 innocents
-- Rule: 2 row 3 innocents
parseRule (x:"row":xs) b = read x == count True [Just (r b) | r <- map (parseRule . (xs ++) . (:[]) . map toLower . removeWhiteSpace . concatMap show . row) [1 .. 5]]
parseRule ("only":"column":x:xs) b = all (\f -> f b) (map ((not .) . (parseRule . ((xs ++ ["column"]) ++) . (:[]))) ([ n | n <- ["a","b","c","d"], n /= x]) ++ [parseRule (xs ++ (["column"]++[x]))])
parseRule ("each":"column":xs) b = all ((\f -> f b) . parseRule . ((xs ++ ["column"])++) . (:[])) ["a", "b", "c", "d"]
parseRule (x:"column":xs) b = read x == count True [Just (r b) | r <- map (parseRule . ((xs ++ ["column"])++) . (:[])) ["a", "b", "c", "d"]]
-- One of the 3 criminals in column A is neighboring x
-- Rule: 1 of the 3 criminal in column A is neighbouring B2
parseRule (x:"of":"the":y:xs) b = case break (`elem` ["are", "is"]) xs of
                (stat:xs, _:ys) -> parseRule (y:stat:xs) b && parseRule (x:stat:(xs++["that"]++ys)) b
-- Only one person on the edge has exactly 4 criminal neighbors
-- Rule: Only 1 on edge has 4 criminal neighbour
parseRule ("only":x:xs) b = case break (`elem` ["have", "has"]) xs of
                (xs, _:ys) -> read x == count True (map (\ n -> Just (parseRule (ys++[show n]) b)) (parseSet xs))
-- The two criminals in row 4 are connected
-- Rule: Connected 2 criminals in row 4
parseRule ("connected":x:s:xs) b = parseRule (x:s:xs) b && connected (stb s) (parseSet xs) b
-- There are an equal number of innocent bakers as innocent pilots
-- Rule: Equal innocent A1B2D5 and B1C1
parseRule ("equal":xs) b = case break (== "and") xs of
            (stat:xs, _:ys) -> count (stb stat) (map (getPos b) (parseSet xs)) == count (stb stat) (map (getPos b) (parseSet ys))
-- 1. All of the innocents below X are connected
-- Rule: All innocents connected below C2
-- 2. Everyone between X and Y is a criminal
-- Rule: All criminal between A2 A5
parseRule ("all":s:xs) b | head xs == "connected" = connected (stb s) (parseSet (tail xs)) b
                         | otherwise              = no (parseSet xs) (not (stb s)) b
parseRule ("no":s:xs) b = no (parseSet xs) (stb s) b
parseRule ("odd" :s:xs) b = odd (count (stb s) (map (getPos b) (parseSet xs)))
-- X has an even number of criminal neigbors
-- Rule: even criminal neighbours C1
parseRule ("even":s:xs) b = even (count (stb s) (map (getPos b) (parseSet xs)))
-- 1. There are more than 3 innocents in column B
-- Rule: More than 3 criminal column B
-- 2. There are more criminals in row 2 than any other row
-- Rule: More criminal row 2 than any other row
parseRule ("more":t:x:s:xs) b | all isInt x = count (stb s) (map (getPos b) (parseSet xs)) > read x
                              | otherwise   = case break (== "than") (t:x:s:xs) of
    ([stat, "row", n], [_, "any", "other", "row"]) ->
        count (stb stat) (map (getPos b) (row (read n))) > foldr max 0 [ count (stb stat) (map (getPos b) (row i)) | i <- [1..5], i /= read n]
    (stat:xs, _:ys) ->
        count (stb stat) (map (getPos b) (parseSet xs)) > count (stb stat) (map (getPos b) (parseSet ys))
parseRule ("less":t:x:s:xs) b | all isInt x = count (stb s) (map (getPos b) (parseSet xs)) < read x
                              | otherwise   = case break (== "than") (t:x:s:xs) of
    ([statx, "row", n], [_, staty, "any", "other", "row"]) ->
        count (stb statx) (map (getPos b) (row (read n))) < foldr max 0 [ count (stb staty) (map (getPos b) (row i)) | i <- [1..5], i /= read n]
    (statx:xs, _:staty:ys) ->
        count (stb statx) (map (getPos b) (parseSet xs)) < count (stb staty) (map (getPos b) (parseSet ys))
-- X has exactly three innocent neighbors on the edges
-- Rule: 3 innocent neighbours B4 that on edge
parseRule (x:s:xs) b | all isInt x = count (stb s) (map (getPos b) (parseSet xs)) == read x

parseSet :: [String] -> [Pos]
parseSet ["on", "edge"] = edge
parseSet ("in":xs) = parseSet xs
parseSet [x] = readPoses x
parseSet (a:b:"that":xs) = parseSet [a, b] `intersect` parseSet xs
parseSet (a:b:c:"that":xs) = parseSet [a, b, c] `intersect` parseSet xs
parseSet ['n':'e':'i':'g':'h':'b':'o':_, x] = foldr1 intersect (map neighbours (readPoses x))
parseSet ["between", x, y] = between (head (readPoses x)) (head (readPoses y))
parseSet ["right", x] = concatMap (dir 0) (readPoses x)
parseSet ["above", x] = concatMap (dir 1) (readPoses x)
parseSet ["left" , x] = concatMap (dir 2) (readPoses x)
parseSet ["below", x] = concatMap (dir 3) (readPoses x)
parseSet ["directly","right", x] = concatMap (dir' 0) (readPoses x)
parseSet ["directly","above", x] = concatMap (dir' 1) (readPoses x)
parseSet ["directly","left" , x] = concatMap (dir' 2) (readPoses x)
parseSet ["directly","below", x] = concatMap (dir' 3) (readPoses x)
parseSet ["row", x] | all isInt x = row (read x)
parseSet ["column", "a"] = colA
parseSet ["column", "b"] = colB
parseSet ["column", "c"] = colC
parseSet ["column", "d"] = colD

-- IO

look :: [[Maybe Bool]] -> [[[Maybe Bool]] -> Bool] -> String -> IO ()
look b rs m = case breakList (map toLower m) ' ' of
    ('q':('u':('i':('t':_)))):_ -> return ()
    ('h':('e':('l':('p':_)))):_ -> putStrLns [
                "'Quit' quits the game",
                "'Help' shows this help",
                "'Rule:' adds a rule",
                "'Innocent x'/'Criminal x' declares someone as such"
                ]
    "rule:":m' -> loop (applyRules (possibilities b) rs') rs' where rs' = parseRule m' : rs
    [x, y] -> loop (mutPoss (map (updateBoard b (stb x)) (readPoses y))) rs
    m' -> case dropWhile (/= '|') (concat m') of
        []     -> getInput b rs
        (x:xs) -> look b rs xs

getInput :: [[Maybe Bool]] -> [[[Maybe Bool]] -> Bool] -> IO ()
getInput b rs = putStr ">> " >> getLine >>= look b rs

loop :: [[Maybe Bool]] -> [[[Maybe Bool]] -> Bool] -> IO ()
loop b rs = putStrLn "" >> print (Display b) >> getInput b rs

main :: IO ()
main = loop board []

{- 2025;12;30
Innocent C2
Rule: 1 innocent between C1 C5 that neighbour D2
Rule: 1 innocent right A2
Rule: 1 innocent directly above C3A5
Rule: connected 2 innocent above C5
Rule: Only row 1 2 criminal
Rule: Each column more than 2 criminal
Rule: All innocent connected in column D
Rule: 1 of the 2 innocents neighbouring C5 is in row 5
Rule: Equal innocent neighbour D1 and neighbour C4
Rule: Odd innocent on edge that neighbour C4
Rule: More criminal row 3 than any other row
Rule: More criminal A3B3D1 than A5C3
Rule: 1 criminal B5
Rule: 1 column 2 innocent

2025;12;31
Innocent C4
Rule: 1 of the 2 criminal right A3 are C3
Rule: No innocent neighbour B2 that neighbour A4
Rule: Only column A 3 criminal
Rule: Connected 2 criminal above A5
Rule: 2 innocent above D5
Rule: 2 of the 3 innocent neighbour C2 are column D
Rule: 1 of the 6 innocent on edge is neighbour C1
Rule: 1 of the 2 innocent neighbour C1 is row 1
Rule: Odd innocent row 2
Rule: Equal innocent neighbour C5 and neighbour C1

2025;12;29 - Evil Bonus Puzzle
Innocent C4
Rule: No innocent neighbour D2 that neighbour D4
Rule: 2 of the 4 innocent neighbour A2 are left D3
Rule: Odd criminal column D
Rule: Equal innocent neighbour A1 and neighbour D3
Rule: Connected 2 criminal below D1
Rule: 1 of the 2 innocent neighbour B1 is neighbour A2
Rule: Odd criminal above A5
Rule: Each column more than 1 criminal
Rule: More innocent neighbour D4 than neighbour A1
Rule: Equal innocent column B and column C
Rule: More innocent row 1 than row 4
Rule: 1 row 3 innocent

2026;1;1
Innocent B1
Rule: 1 of the 3 innocents below D1 is neighbouring C2
Rule: 3 innocents below B1
Rule: Only column C 2 criminals
Rule: 2 of the 6 innocents neighbouring C2 are in column C
Rule: 1 of the 5 innocents neighbouring C3 is D2
Rule: 1 of the 2 criminals neighbouring B2 is C2
Rule: Equal criminal neighbours A4 and neighbours C1
Rule: Odd criminals neighbouring A3

2026;1;4
Innocent A4
Rule: 2 of the 4 innocents neighbouring D4 are in row 5
Rule: 1 of the 2 criminals above B5 is B4
Rule: Odd innocents column B
Rule: 3 innocent neighbours D3
Rule: 1 of the 5 innocents neighbouring C4 is neighbour D2
Rule: 1 of the 2 innocents neighbouring B2 is above B4
Rule: 3 criminal neighbour A2
Rule: Equal innocent neighbours A2 and neighbours D2
Rule: More innocent neighbours A2 than neighbours D1
Rule: Equal criminal neighbours D1 and neighbours B5
Rule: 1 innocent between A2 D2

2026;1;12
Innocent D4
Rule: 2 of the 4 innocents neighbouring A2 are in column B
Rule: 1 of the 2 innocents in column D is D2
Rule: 2 innocents left D3 that neighbour B3
Rule: Equal innocent A2A3 and B1B2B3
Rule: Connected 2 criminals in row 4
Rule: 1 innocent directly left D2D3
Rule: 1 innocent below B3
Rule: 
-}