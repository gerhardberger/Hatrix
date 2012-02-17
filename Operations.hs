module Operations ( 
			swap,
			matrixMap,
			multiply,
			add,
			sub
				)
	where

import Array

-- Maps through the elems of a matrix
matrixMap'' arr f = listArray (bounds arr) $ map f $ elems arr

matrixMap' l f m = m //
	[(l, matrixMap'' (m ! l) f)]

matrixMap :: (Ix i, Ix i1, Ix i2, Num a)=> i2 -> (a -> a) -> (Array i2 (Array i a), Array i2 (Array i1 a)) -> (Array i2 (Array i a), Array i2 (Array i1 a))
matrixMap l f (p,q) = (matrixMap' l f p, matrixMap' l f q)



-- Swapping two lines
swap' a b m = m //
	[(a, m ! b), (b, m ! a)]

swap :: Ix i => i -> i -> (Array i e, Array i e1) -> (Array i e, Array i e1)
swap a b (p,q) = (swap' a b p, swap' a b q)

-- Multiplies by n the elems of lth line
multiply :: (Ix i, Ix i1, Ix i2, Num a)=> i2 -> a -> (Array i2 (Array i a), Array i2 (Array i1 a)) -> (Array i2 (Array i a), Array i2 (Array i1 a))
multiply l n = matrixMap l (*n)

-- Add n to the elems of lth line
add :: (Ix i, Ix i1, Ix i2, Num a)=> i2 -> a -> (Array i2 (Array i a), Array i2 (Array i1 a)) -> (Array i2 (Array i a), Array i2 (Array i1 a))
add l n = matrixMap l (+n)

-- Substracts n from the elems of lth line
sub :: (Ix i, Ix i1, Ix i2, Num a)=> i2 -> a -> (Array i2 (Array i a), Array i2 (Array i1 a)) -> (Array i2 (Array i a), Array i2 (Array i1 a))
sub l n = matrixMap l (+(-n))