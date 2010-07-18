module Amata.Test
( main
,	simpleScore
) where

import System.Random
import Control.Monad
import Amata.Automata
import Amata.Train

simpleScore t =
	case t of
		("T","F") -> (1,0)
		("F","T") -> (0,1)
		("F","F") -> (-1,-1)
		("T","T") -> (2,2)
		_ -> error "error"

main :: IO ()
main = do
	pop <- mapM (\f -> buildAmata 8 ["T","F"] ["T","F"]) [0..100]
	mutate <- mapM (\f -> mutateAmata f ["T","F"] ["T","F"]) pop
	let res = map (\f -> runAmataList f 0 ["T","F","T"]) mutate 
	let mfst = (mutate !! 0) 
	let msnd = (mutate !! 1) 
	let play = beginGame mfst msnd simpleScore 10 
	mapM_ (\f -> putStrLn $ show f) res
	putStrLn $ show play

