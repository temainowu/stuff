import GHC.Integer (eqInteger, integerToInt)
import GHC.Real (Integral)
import Prelude (Fractional)

f :: Fractional a => a -> a -> a
f a b = a + (1 / b)

parse :: Fractional p => [p] -> p
parse = foldr f 0

-- array = []
-- array.append(int(x))
-- for i in [2..infinity]:
--     for j in range(i-1):
--         if x - (1/i) <= 0:
--             array.append(i)

fracToList :: t1 -> [a]
fracToList frac = boundedFracToList frac 1

findVal :: t1 -> t2 -> t3
findVal frac window = toInteger (div frac window)

boundedFracToList :: t1 -> t2 -> [a]
boundedFracToList frac window
  | window < 0.00000001 = [findVal frac window]
  | otherwise = findVal frac window : boundedFracToList (findVal frac window) (window / 10)