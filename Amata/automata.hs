module Amata.Automata
( Amata(..)
, State(..)
, Edge(..)
, buildAmata
, runAmata
, runAmataList
, mutateAmata
) where
 
import System.Random
import Control.Monad
import Amata.Utilities

data Edge a = Edge {transition :: a, destination :: Int } deriving Show
data State a b = State {value :: a, edges :: [Edge b]} deriving Show
data Amata a b = Amata {name :: String, states :: [State a b]} deriving Show

getState :: Amata a b -> Int -> Maybe (State a b)
getState fa idx = 
	let l= length (states fa) in
	if idx < l then Just ((states fa) !! idx) else Nothing

getEdge :: Amata a b -> Int -> Int -> Maybe (Edge b)
getEdge fa s e =
	let st = getState fa s in
	case st of
		Just nst ->
			let l = length (edges nst) in
			if e < l then Just ((edges nst) !! e) else Nothing
		Nothing -> Nothing

replaceState :: Amata a b -> Int -> State a b -> Amata a b
replaceState fa idx s =
	let curr_st = getState fa idx in
	let all = states fa in
	case curr_st of 
		Just st ->
			let new = replace all idx s in
			Amata (name fa) new
		Nothing ->
			fa

insertState :: Amata a b -> Int -> State a b -> Amata a b
insertState fa idx s =
	let all = states fa in
	let new_all = insert all idx s in
	Amata (name fa) new_all

deleteState :: Amata a b -> Int -> Amata a b
deleteState fa idx =	
	let all = states fa in
	Amata (name fa) (delete all idx)

replaceValue :: Amata a b -> Int -> a -> Amata a b
replaceValue fa idx val =
	let curr_st = getState fa idx in
	case curr_st of
		Just st ->
			let c_edges = edges st in
			replaceState fa idx (State val c_edges)
		Nothing -> fa

replaceEdge :: Amata a b -> Int -> Int -> Edge b -> Amata a b
replaceEdge fa s_idx e_idx e =
	let curr_st = getState fa s_idx in
	case curr_st of
		Just st ->
			let curr_ed = getEdge fa s_idx e_idx in
			let all_e = edges st in
			case curr_ed of
				Just ed ->
					let new = replace all_e e_idx e in
					replaceState fa s_idx (State (value st) new)
				Nothing -> fa
		Nothing -> fa			

insertEdge :: Amata a b -> Int -> Int -> Edge b -> Amata a b
insertEdge fa s_idx e_idx e =
	let curr_st = getState fa s_idx in
	case curr_st of
		Just st ->
			let all_e = edges st in
			let new_e = insert all_e e_idx e in
			replaceState fa s_idx (State (value st) new_e)
		Nothing -> fa

deleteEdge :: Amata a b -> Int -> Int -> Amata a b
deleteEdge fa s_idx e_idx =	
	let curr_st = getState fa s_idx in
	case curr_st of
		Just st ->
			let all_e = edges st in
			let del_e = delete all_e e_idx in
			replaceState fa s_idx (State (value st) del_e)
		Nothing -> fa

runAmata :: (Eq b) => (Amata a b) -> Int -> b -> Maybe (a,Int)
runAmata fa curr_state new_input =
	let c_state = getState fa curr_state in
	case c_state of
		Just (State c_val c_edges) ->
			let validEdge = filter (\f -> transition f == new_input) c_edges in
				case validEdge of
					(Edge trans dest):rest -> 
						let next_state = getState fa dest in
							case next_state of
									Just (State n_val n_edges) -> Just (n_val,dest)
									Nothing -> Just (c_val, curr_state)
					[] -> Just (c_val, curr_state)
		Nothing -> Nothing

runAmataList :: (Eq b) => (Amata a b) -> Int -> [b] -> Maybe (a, Int)
runAmataList fa curr_state inputlist =
	let start_val = value $ states fa !! 0 in 
		foldl (\acc g -> 
			case acc of
				Just (v,s) -> runAmata fa s g
				Nothing -> Nothing) (Just (start_val,0)) inputlist

buildEdge :: Int -> [b] -> IO (Edge b)
buildEdge num_states lang = do
	dest <- pickNum 0 (num_states - 1)
	trans <- chooseOfList lang
	return $ Edge trans dest

buildEdgeList :: Int -> Int -> [b] -> IO [Edge b]
buildEdgeList num_states num_edges lang = 
	mapM (\e -> buildEdge num_states lang) [1..num_edges]

buildState :: Int -> [a] -> [b] -> IO (State a b)
buildState num_states vals lang = do
	let len = length lang
	new_v <- chooseOfList vals
	num_e <- pickNum 1 len
	new_el <- buildEdgeList num_states num_e lang
	return $ State new_v new_el 

buildAmata :: Int -> [a] -> [b] -> IO (Amata a b)
buildAmata state_size vals lang = do
	how_big <- (pickNum 1 $ state_size -1)
	let states_to_be = [0..how_big]
	states <- mapM (\s -> buildState state_size vals lang) states_to_be
	return $ Amata "Random" states						

mutateAmata :: (Amata a b) -> [a] -> [b] -> IO (Amata a b)
mutateAmata fa vals lang = do
	let size = length (states fa) 
	on_state <- pickNum 0 (size-1)
	choice <- pickNum 1 6
	case choice of
		1 -> do -- Insert State
			new_s <- buildState size vals lang
			return $ insertState fa size new_s
		2 -> do -- delete State
			return $ deleteState fa on_state
		3 -> do -- insert Edge
			let edge_len = length $ edges $ (states fa) !! on_state
			on_edge <- pickNum 0 (edge_len-1)
			new_e <- buildEdge (size-1) lang
			return $ insertEdge fa on_state on_edge new_e
		4 -> do -- delete Edge
			let edge_len = length $ edges $ (states fa) !! on_state
			on_edge <- pickNum 0 (edge_len-1)
			return $ deleteEdge fa on_state on_edge
		5 -> do -- change state value
			new_val <- chooseOfList vals
			return $ replaceValue fa on_state new_val
		6 -> do -- replace edge
			let edge_len = length $ edges $ (states fa) !! on_state
			on_edge <- pickNum 0 (edge_len-1)
			new_e <- buildEdge (size-1) lang
			return $ replaceEdge fa on_state on_edge new_e
				
			
			
