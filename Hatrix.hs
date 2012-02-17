module Hatrix (
			makeMatrixArray,
			makeIdentity
				)
	where

import Array
import qualified Operations as Ops

makeMatrixArray :: [[e]] -> Array Int (Array Int e)
makeMatrixArray m = listArray (1,length m) $ map (\a -> listArray (1, length a) a) m

makeIdentity' :: (Enum a, Num a, Num e, Ix a) => (a,a) -> a -> Array a e
makeIdentity' b n = array b [(i, (\a -> if a == n
						then 1
						else 0) i) | i <- [1..snd b]]

makeIdentity :: (Enum a, Num a, Num e, Ix a) => (a,a) -> Array a (Array a e)
makeIdentity b = array b [(i, makeIdentity' b i) | i <- [1..snd b]]