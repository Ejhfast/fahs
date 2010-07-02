import System.Random
import Control.Monad
import Amata.Automata

main :: IO ()
main = do
	pop <- mapM (\f -> buildAmata 8 ["T","F"] [0..8]) [0..100]
	mutate <- mapM (\f -> mutateAmata f ["T","F"] [0..8]) pop
	let res = map (\f -> runAmataList f 0 [1,1,1,0,0,0,1,0]) mutate
	mapM_ (\f -> putStrLn $ show f) res

