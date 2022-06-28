import Control.Monad.Writer.Strict
import Data.Kind (Constraint)
import qualified GHC.Types
import Prelude hiding (sum)

sum :: Num a => [a] -> a
sum = foldr (+) 0

pro :: Num a => [a] -> a
pro = foldr (*) 1

ln :: Fractional a => a -> a
ln x = (x ^ h - 1) / h

h :: Fractional a => a
h = 1 / 2

pow :: (Integral a, Fractional a) => a -> a -> a
pow x e = x ^ ln e

exr :: (Integral a, Fractional a) => [a] -> a
exr = foldr pow 2