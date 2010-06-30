data Edge a = Edge {transition :: a, destination :: a } deriving Show
data State a b = State {value :: a, edges :: [Edge b]} deriving Show
data Amata a b = Amata {name :: String, states :: [State a b]} deriving Show

replace :: [a] -> Int -> a -> [a]
replace lst idx e =
	let (f,r) = splitAt idx lst in
	let rest = drop 1 r in
	if idx < length lst then f ++ [e] ++ rest else lst

mapvals :: Amata a b -> [a] 
mapvals fa = map (\f -> value f) (states fa)

mapedges :: Amata a b -> [[Edge b]]
mapedges fa = map (\f -> edges f) (states fa)

mapTransitions :: Amata a b -> [[b]]
mapTransitions fa = map (\f -> map (\g -> transition g) f) (mapedges fa)

mapDestinations :: Amata a b -> [[b]]
mapDestinations fa = map (\f -> map (\g -> destination g) f) (mapedges fa)

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
				Nothing ->
					fa
		Nothing ->
			fa			

		
