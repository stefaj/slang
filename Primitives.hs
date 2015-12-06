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
				("++", strApp),
				("++=", strInter),
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

strInter :: SFunction
strInter (a:args) = String $ (intercalate $ unpackStr a) $ (map unpackStr args) 

strApp :: SFunction
strApp args = String $ foldr1 (++) $ (map unpackStr args)

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

--eval env (ExecFunc "let" [(Atom funcName), expr]) = defineVar funcName expr env
eval env (BindLet funcName expr) = do 
	localEnv <- copyEnv env
	evaled <- eval localEnv expr
	defineVar funcName evaled env
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
						(Just v) -> return v
						Nothing -> return $ Error ("Var " ++ show a ++ " not found" )  -- return $ Atom a

eval env (Sequence []) = return Empty
eval env (Sequence xs) = do --first we need to make a local environment\
			localEnv <- copyEnv env
			res <- mapM (eval localEnv) xs
			eval localEnv (last res)
--		| UserFunc String [String] SExpr
--eval env (UserFunc funcName []

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