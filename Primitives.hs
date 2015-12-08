module Primitives(basicFunctions, eval, basicBindingsEnv, Env, getVar',apply, printEnv, load, sequenceAll') where

import Types
import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Maybe
import Data.IORef
import Parser
import System.IO

basicFunctions = [
			--	("<", numericBoolBinOp (<)),
				("+",numericBinOp (+)),
				("-",numericBinOp (-)),
				("*",numericBinOp (*)),
				("/",numericBinOp (/)),
				("<", numericBoolBinOp (<)),
				(">", numericBoolBinOp (>)),
				("++", concatList),
				("++=", intercalateList),
				("@", getAtOp),
				("head", headOp),
				("tail", tailOp),
				("drop", dropOp),
				("take", takeOp),
				(":", consOp),
				("==", equals),
				("ToNum", toNum),
				("ShowVal", showVal)]

ioFunctions = [("ReadToEnd", readContents),
				("OpenFileR", makePort ReadMode),
				("OpenFileW", makePort WriteMode),
				("WriteLine", writeLineFunc),
				("ReadLine", readLineFunc)]


basicBindingsEnv :: IO Env
basicBindingsEnv = nullEnv >>= (flip bindVars $ (map makePrim basicFunctions ++ (map makeIOPrim ioFunctions)))
	where 
		makePrim (var, func) = (var, Func func)
		makeIOPrim (var, func) = (var, IOFunc func)


equals :: [SExpr] -> SExpr
equals args = if (length args == 2)
	then Boolean $ (args !! 0) == (args !! 1)
	else Error $ "Only 2 arguments expected; got " ++ (show $ length args) 


-- COME IO, TO THE HEAVENS!!
makePort :: IOMode -> [SExpr] -> IO SExpr
makePort mode [String filename] = do (openFile filename mode) >>= (return . Handle)

closePort :: [SExpr] -> IO SExpr
closePort [Handle p] = do
	hClose p
	return $ Boolean True
closePort _ = return $ Boolean False


readLineFunc :: [SExpr] -> IO SExpr
readLineFunc [Handle p] = do
	line <- hGetLine p
	return $ String line

writeLineFunc :: [SExpr] -> IO SExpr
writeLineFunc [Handle p, dat] = do
	hPrint p dat
	return $ Boolean True

readContents :: [SExpr] -> IO SExpr
readContents [String filename] = do
	contents <- readFile filename
	return $ String contents

toNum :: [SExpr] -> SExpr
toNum [String s] = Number (read s)
toNum [Number n] = Number n
toNum _ = Error $ "This type cannot be converted"

showVal :: [SExpr] -> SExpr
showVal [x] = String $ show x

apply :: String -> Env -> [SExpr] -> IO SExpr
apply funcName env args = do
	res <- getVar' funcName env
	case res of 
		(Just (IOFunc func)) -> func args
		(Just (Func func)) -> return $ func args
		(Just (UserFunc argNames func)) -> if (length args /= (length argNames)) 
			then return $ Error ("Func args do not match; expected" ++ (show (length argNames)) ++ " found" ++ (show (length args)) )
			else do
				newE <- bindVars env (zip argNames args)
				func' <- eval newE func
				return func'
		(Just s@(Atom a)) -> return $ s
		otherwise -> return $ Error ("Func " ++ funcName ++ " not found" )  -- return $ Atom a



-- type SFunction = [SExpr] -> SExpr 

sPlus :: SFunction
sPlus args = numericBinOp (+) args 
sMin args = numericBinOp (+) args 



numericBinOp :: (NumericType -> NumericType -> NumericType) -> [SExpr] -> SExpr 
numericBinOp op [List xs] = numericBinOp op xs
numericBinOp op args = Number $ foldr1 op $ map unpackNum args

--numericBoolBinOp op args = Boolean True
numericBoolBinOp :: (NumericType -> NumericType -> Bool) -> [SExpr] -> SExpr 
numericBoolBinOp op [List xs] = numericBoolBinOp op xs
numericBoolBinOp op args = let nums = map unpackNum args in
	if (length args == 2)
		then Boolean $ op (nums !! 0) (nums !! 1)
		else Error $ "Only 2 arguments expected; got " ++ (show $ length args) 

-- unpacking
unpackNum :: SExpr -> NumericType
unpackNum (Number n) = n
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0 -- error later

unpackStr :: SExpr -> String
unpackStr (String n) = n
unpackStr (List [n]) = unpackStr n
unpackStr _ = ""

unpackList :: SExpr -> [SExpr]
unpackList (List xs) = xs
unpackList _ = []


readOrThrow parser = parse parser "sl"
readExp3 = readOrThrow parseExpr
readExpList = readOrThrow parseMain

load :: String -> IO [SExpr]
load filename = do
 		fileContents <- readFile filename 
		let v = (readExpList fileContents)
		case v of
			Left _ -> return []
			Right sxprs -> return sxprs

readAll :: String -> IO SExpr
readAll filename = do
			sxprs <- load filename
			return $ Sequence sxprs

sequenceAll :: Env -> IO [SExpr] -> IO SExpr
sequenceAll env sqs = do
	sequences <- sqs
	evaled <- mapM (eval env) sequences
	return $ case (length evaled) of
		0 -> Empty
		otherwise -> last evaled

sequenceAll' env filename = sequenceAll env (load filename)



-- Basic Functional operations ------------------------------------------------------------------------------
--mapOp :: [SExpr] -> SExpr
--mapOp []






-- List operations ------------------------------------------------------------------------------

consOp :: [SExpr] -> SExpr
consOp [x, List xs] = List (x:xs)
consOp _  = Error $ "Required arguments is expression and a list"


getAtOp :: [SExpr] -> SExpr
getAtOp [String xs, Number n] = String $ (xs !! (truncate n)):""
getAtOp [List xs, Number n] = xs !! (truncate n)
getAtOp _ = Error "Require a list"

headOp :: [SExpr] -> SExpr
headOp [List xs] = head xs
headOp [String xs] = String $ (head xs):""
headOp _ = Error "Require a list"

tailOp :: [SExpr] -> SExpr
tailOp [List xs] = List (tail xs)
tailOp [String xs] = String (tail xs)
tailOp _ = Error "Require a list"

dropOp :: [SExpr] -> SExpr
dropOp [Number n, String xs] = String $ (drop (truncate n) xs)
dropOp [Number n, List xs] = List $ (drop (truncate n) xs)
dropOp _ = Error "Require a list"

takeOp :: [SExpr] -> SExpr
takeOp [Number n, String xs] = String $ (take (truncate n) xs)
takeOp [Number n, List xs] = List $ (take (truncate n) xs)
takeOp _ = Error "Require a list"


concatList :: [SExpr] -> SExpr
concatList xs = foldr1 aux xs 
	where
		aux :: SExpr -> SExpr -> SExpr
		aux (String xs) (String ys) = String (xs ++ ys)
		aux (List xs) (List ys) = List (xs ++ ys)
		aux _ _ = List []

intercalateList :: [SExpr] -> SExpr
intercalateList [List xs, List xss] = List $ (intercalate xs $ map unpackList xss) 
intercalateList [String s, List xs] = String $ intercalate s $ map unpackStr xs
intercalateList _ = Error "Invalid arguments"

-- assume initial is evaled
loopHelper :: Env -> String -> SExpr -> SExpr -> IO SExpr
loopHelper env varName condition body = do
	body' <- eval env body
	setVar env varName body'
	cond' <- eval env condition
	case cond' of
		(Boolean True) -> do
			loopHelper env varName condition body
		otherwise -> return body'

-- Evaluation

eval :: Env -> SExpr -> IO SExpr
eval env v@(Number _) = return v
eval env v@(String _) = return v
eval env v@(Boolean _) = return v
eval env (List [s]) = eval env s
eval env (If cond conseq altern) = do
	cond' <- eval env cond
	case cond' of
		(Boolean True) -> eval env conseq
		otherwise -> eval env altern
-- WhileDo String SExpr SExpr SExpr -- VarName, InitialValue, Condition, Body
eval env (WhileDo varName initial condition body) = do
	localEnv <- copyEnv env
	evaledInitial <- eval localEnv initial
	defineVar varName evaledInitial localEnv
	loopHelper localEnv varName condition body

--eval env (ExecFunc "let" [(Atom funcName), expr]) = defineVar funcName expr env
eval env (BindLet funcName expr) = do 
	defineVar funcName expr env
	return $ Empty
	--localEnv <- copyEnv env
	--evaled <- eval localEnv expr
	--defineVar funcName evaled env

eval env v@(BindFunc funcName args expr) = do
			let ufunc = UserFunc args expr
			defineVar funcName ufunc env
			return v 

eval env (ExecFunc "import" [String filename]) = (sequenceAll' env filename) >>= eval env 
eval env (ExecFunc s args) = do
			localEnv <- copyEnv env
			evaled_args <- mapM (eval localEnv) args
			func <- apply s localEnv evaled_args
			return func
			--eval localEnv func
eval env (List []) = return Empty
eval env (List xs) = do
			res <- mapM (eval env) xs
			return $ List res
eval env (Atom "state") = do
	p <- printEnv env
	return $ String p 
eval env (Atom "stdin") = return $ Handle stdin
eval env (Atom "stdout") = return $ Handle stdout
eval env (Atom a) = do
					res <- getVar' a env
					case res of 
						(Just v) -> eval env v	 --return v
						Nothing -> return $ Error ("Var " ++ show a ++ " not found" )  -- return $ Atom a

eval env (Sequence []) = return Empty
eval env (Sequence xs) = do --first we need to make a local environment\
			localEnv <- copyEnv env
			res <- mapM (eval localEnv) xs
			eval localEnv (last res)

eval env (BindListGen start Empty inc) = do
	s' <- eval env start >>= (return . unpackNum)
	i' <- eval env inc >>= (return . unpackNum)
	return $ List $ map Number [(s'),(s' + i')..]

eval env (BindListGen start finish inc) = do
	s' <- eval env start >>= (return . unpackNum)
	e' <- eval env finish >>= (return . unpackNum)
	i' <- eval env inc >>= (return . unpackNum)
	return $ List $ map Number [(s'),(s' + i')..(e')]




eval env s = return s





printEnv :: Env -> IO String
printEnv env = do
	state <- readIORef env
	printEnvB state

printEnvB :: [(String, IORef SExpr)] -> IO String
printEnvB [] = return $ ""
printEnvB ((a,ioRef):xs) = do
	sexpr <- readIORef ioRef
	next <- printEnvB xs
	return $ "(" ++ a ++ ", " ++ (show sexpr) ++ "), " ++ next

-- Mutation

type Env = IORef [(String, IORef SExpr)]

nullEnv :: IO Env
nullEnv = newIORef []

--defineVar :: String -> SExpr -> Env a -> ST a SExpr
defineVar var value stateRef = do
	-- valueRef <- newSTRef value
	valueRef <- newIORef value
	state <- readIORef stateRef
	writeIORef stateRef $ (var,valueRef) : state
	return value


setVar :: Env -> String -> SExpr -> IO SExpr
setVar envRef var value = do 
			env <- readIORef envRef
			let varRef = fromJust $ lookup var env
			writeIORef varRef value
			return value


bindVars :: Env -> [(String, SExpr)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)


copyEnv :: Env -> IO Env
copyEnv stateRef = do
	state <- readIORef stateRef
	newState <- newIORef state
	return newState
		

--getVar' :: String -> Env a -> ST a (Maybe SExpr)
getVar' :: String -> Env -> IO (Maybe SExpr)
getVar' var stateRef = do
	state <- readIORef stateRef
	let varRef = lookup var state
	case varRef of
		Just v -> do
			sexpr <- readIORef v
			return $ Just $ sexpr
			 -- return $ Just $ readIORef v
		Nothing -> return $Nothing
	

--getVar :: Env -> String -> (Maybe SPECIALIZE)
--getVar envRef var  =  do env <- liftIO $ readIORef envRef
--                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
--                               (liftIO . readIORef)
--                               (lookup var env)