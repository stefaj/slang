import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Maybe

--type Env = STRef s [(String, Float)]

--nullEnv :: ST s (Env s)
nullEnv = newSTRef []

--defineVar :: String -> Float -> Env a -> ST a Float
defineVar var value stateRef = do
	-- valueRef <- newSTRef value
	state <- readSTRef stateRef
	writeSTRef stateRef $ (var,value) : state
	readSTRef stateRef


bindVars stateRef bindings = do
		state <- readSTRef stateRef
		let newState = bindings ++ state
		newSTRef newState
		



--getVar' :: String -> Env s -> ST s (Maybe Float)
getVar' var stateRef = do
	state <- readSTRef stateRef
	let value = fromJust $ lookup var state 
	return value

--getVar :: String -> Env s -> Maybe Float
--getVar var stateRef = runST $ getVar' var stateRef


testLoop = runST $ do
	n <- newSTRef "aha"
	modifySTRef n (++"hey")
	readSTRef n 


testLoop2 = runST $ do
	n <- newSTRef []
	defineVar "hey" 10 n
	readSTRef n



once = runST $ do
	n <- newSTRef []
	defineVar "H" 10 n
	defineVar "A" 5 n
	readSTRef n
	a <- getVar' "H" n
	return a







--testLoop = do
--	let n <- nullEnv
--	defineVar "h" 10 n
--	defineVar "k" 20 n
--	return $ getVar "h"

--setVar env (s,f) = do
--		inst <- lookup
--		modifySTRef env $ do
--			modifySTRef  

--test = do
--	a <- newSTRef 0
--	a <- newSTRef 2
--	readSTRef a