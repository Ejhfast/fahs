module Amata.Utilities
( getPercent
, pickNum
, chooseOfList
, replace
, insert
, delete
) where
 
import System.Random

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

replace :: [a] -> Int -> a -> [a]
replace lst idx e =
	let (f,r) = splitAt idx lst in
	let rest = drop 1 r in
	if idx < length lst then f ++ [e] ++ rest else lst

insert :: [a] -> Int -> a -> [a]
insert lst idx e =
	let (f,r) = splitAt idx lst in
	f ++ [e] ++ r

delete :: [a] -> Int -> [a]
delete lst idx =
	let (f,r) = splitAt idx lst in
	f ++ (drop 1 r)

