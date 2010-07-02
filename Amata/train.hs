module Amata.Train
( evalOnFunc
) where

import Amata.Automata

evalOnFunc :: (Eq a) => (Eq b) => (Amata a b) -> ([b] -> a) -> [[b]] -> Int
evalOnFunc fa func input =
	let desired = map func input in
	let actual = map 
		(\f -> 
			let res = runAmataList fa 0 f in
			case res of
				Just (v,s) -> v
				Nothing -> error "Amata Failed to Run"	) 
		input in
	let pair = zip desired actual in
	length $ filter (\(d, a) -> d == a) pair
