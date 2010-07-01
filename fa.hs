import System.Random

data Edge a = Edge {transition :: a, destination :: Int } deriving Show
data State a b = State {value :: a, edges :: [Edge b]} deriving Show
data Amata a b = Amata {name :: String, states :: [State a b]} deriving Show

-- main

main :: IO ()
main = do
	x <- getPercent 50 
	y <- probFilter 50 [1,2,3,5] []
	if x then putStrLn "Hey," else putStrLn "Wha,"
	putStrLn "Hello World\n"

-- utilities

getPercent :: Int -> IO Bool
getPercent x = do
	y <- getStdRandom (randomR (1,100))
	return $ y < x

pickNum :: Int -> Int -> IO Int
pickNum min max = do
	y <- getStdRandom (randomR (min,max))
	return y

chooseOfList :: [a] -> IO a
chooseOfList lst = do
	let len = length lst 
	choice <- pickNum 0 $ (len-1)
	return $ lst !! choice

probFilter :: Int -> [a] -> [a] -> IO [a]
probFilter prob [] acc = return acc
probFilter prob (x:xs) acc = do
	yn <- getPercent prob
	case yn of
		True -> probFilter prob xs (x:acc) 
		False -> probFilter prob xs acc

mapIO :: (a -> IO b) -> [a] -> [b] -> IO [b]
mapIO f [] acc = do return acc
mapIO f (x:xs) acc = do
	new <- f x
	mapIO f xs (new:acc)

replace :: [a] -> Int -> a -> [a]
replace lst idx e =
	let (f,r) = splitAt idx lst in
	let rest = drop 1 r in
	if idx < length lst then f ++ [e] ++ rest else lst

-- Fa functions

mapvals :: Amata a b -> [a] 
mapvals fa = map (\f -> value f) (states fa)

mapedges :: Amata a b -> [[Edge b]]
mapedges fa = map (\f -> edges f) (states fa)

mapTransitions :: Amata a b -> [[b]]
mapTransitions fa = map (\f -> map (\g -> transition g) f) (mapedges fa)

mapDestinations :: Amata a b -> [[Int]]
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


buildAmata :: Int -> [a] -> [b] -> IO (Amata a b)
buildAmata state_size vals lang = do
	how_big <- pickNum 1 state_size
	let states_to_be = [0..(how_big-1)]
	states <- mapIO 
		(\s -> do
			value <- chooseOfList vals
			new_edges <- probFilter 50 lang []
			e_l <- mapIO 
				(\n -> do
					d <- pickNum 0 (state_size -1)
					return $ Edge n d) new_edges []
			return $ State value e_l) states_to_be []
	return $ Amata "Random" states
						
	
