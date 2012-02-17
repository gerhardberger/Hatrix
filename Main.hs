module Main
	where

import IO
import Array
import qualified Hatrix as H
import qualified Operations as Ops

convert'' x = map (map (\y -> read y::Int)) x
convert' x = map words $ lines x

convert :: String -> [[Int]]
convert x = convert'' $ convert' x

exec l n m = (map elems $ (elems . fst) p, map elems $ (elems . snd) p)
	where
		p = Ops.multiply l n (H.makeMatrixArray m, H.makeIdentity (1, length m))


main = do
	hSetBuffering stdin LineBuffering
	s <- readFile "example.txt"
	let m = convert s
	return $ exec 1 2 m