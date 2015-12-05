module Primitives(basicFunctions, eval, basicBindingsEnv, Env, getVar',apply, printEnv) where

import Types
import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Maybe
import Data.IORef
import Parser

basicFunctions = [
				("+",numericBinOp (+)),
				("-",numericBinOp (-)),
				("*",numericBinOp (*)),
				("/",numericBinOp (/)),
				("++", strApp),
				("++=", strInter)]


basicBindingsEnv :: IO Env
basicBindingsEnv = nullEnv >>= (flip bindVars $ map makePrim basicFunctions)
	where makePrim (var, func) = (var, Func func)


--apply :: String -> [SExpr] -> SExpr
--apply funcName args = 
--	let func = lookup funcName basicFunctions 
--	in case func of
--		(Just f) -> f args
--		otherwise -> Error $ "Function " ++ funcName ++ " not found"
--



apply :: String -> Env -> [SExpr] -> IO SExpr
apply funcName env args = do
	res <- getVar' funcName env
	case res of 
		(Just (Func func)) -> return $ func args
		(Just (UserFunc argNames func)) -> if (length args /= (length argNames)) 
			then return $ Error ("Func args do not match; expected" ++ (show (length argNames)) ++ " found" ++ (show (length args)) )
			else do
				newE <- bindVars env (zip argNames args)
				func' <- eval newE func
				return func'
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
	return $ last evaled

sequenceAll' env filename = sequenceAll env (load filename)


-- Evaluation

eval :: Env -> SExpr -> IO SExpr
eval env v@(Number _) = return v
eval env v@(String _) = return v
eval env v@(Bool _) = return v
eval env (List [s]) = eval env s
eval env (ExecFunc "import" [String filename]) =  (sequenceAll' env filename) >>= eval env --(readAll filename) >>= (eval env)  
eval env (ExecFunc "let" [(Atom funcName), expr]) = defineVar funcName expr env
eval env v@(BindFunc funcName args expr) = do
			let ufunc = UserFunc args expr
			defineVar funcName ufunc env
			return v 
eval env (ExecFunc s args) = do
			localEnv <- copyEnv env
			evaled_args <- mapM (eval localEnv) args
			func <- apply s localEnv evaled_args
			return func
			--eval localEnv func

eval env (List xs) = do
			res <- mapM (eval env) xs
			return $ List res
eval env (Atom "state") = do
	p <- printEnv env
	return $ String p 
eval env (Atom a) = do
					res <- getVar' a env
					case res of 
						(Just v) -> return v
						Nothing -> return $ Error ("Var " ++ show a ++ " not found" )  -- return $ Atom a
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