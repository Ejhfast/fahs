module Amata.Train
( PlayTrack(..)
, LookupFunc
, evalOnFunc
, playRound
, beginGame
) where

import Amata.Automata

data PlayTrack a = PlayTrack { p1 :: (STrack a, Int), p2 :: (STrack a, Int) }
type LookupFunc a = (a,a) -> (Int,Int)
type Score = (Int,Int)


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

playRound :: (Eq a) => (Amata a a) -> Int -> a -> (Amata a a) -> Int -> LookupFunc a -> PlayTrack a
playRound fa1 s1 in1 fa2 s2 scoreFunc =
	let initTurn = runAmata fa1 s1 in1 in
	case initTurn of
		Just f1p@(v1,st1) -> 
			let next = runAmata fa2 s2 v1 in
			case next of
				Just f2p@(v2,st2) ->
					let (sc1,sc2) = scoreFunc (v1,v2) in
					PlayTrack ((v1,st1),sc1) ((v2,st2),sc2)
				Nothing -> error "Amata Failed to Run"
		Nothing -> error "Amata Failed to Run"

playGame :: (Eq a) => (Amata a a) -> Int -> a -> (Amata a a) -> Int -> LookupFunc a -> Int -> Score -> Score
playGame fa1 s1 in1 fa2 s2 scoreFunc 0 score = score
playGame fa1 s1 in1 fa2 s2 scoreFunc times score =
	let (p1s,p2s) = score in
	let res = playRound fa1 s1 in1 fa2 s2 scoreFunc in
	case res of
		PlayTrack ((v1,new_s1),sc1) ((new_in1,new_s2),sc2) -> 
			let new_score = ((sc1+p1s),(sc2+p2s)) in
			playGame fa1 new_s1 new_in1 fa2 new_s2 scoreFunc (times - 1) new_score
		_ -> error "Failed at playing game"

beginGame :: (Eq a) => (Amata a a) -> (Amata a a) -> LookupFunc a	-> Int -> Score
beginGame fa1 fa2 func times =	
	let a2 = getState fa2 0 in
	case a2 of
		Just (State v2 e2) ->
			playGame fa1 0 v2 fa2 0 func times (0,0)
		Nothing -> error "Error in beginning game."
			
