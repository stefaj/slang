module Types(NumericType, SExpr(..), SFunction) where

import Data.List
import System.IO
-- to easily change it later if necessary
type NumericType = Double

data SExpr = Atom String
		| Number NumericType
		| String String
		| Boolean Bool
		| List [SExpr] 
		| Sequence [SExpr]
--		| Func String [SExpr]
		| ExecFunc String [SExpr]
		| BindFunc String [String] SExpr
		| BindLet String SExpr
		| If SExpr SExpr SExpr
		| IOFunc ([SExpr] -> IO SExpr)
		| Func ([SExpr] -> SExpr)
		| UserFunc [String] SExpr -- function arguments and body
		| Error String
		| Handle Handle
		| Empty



-- maps function with parameters to a SExpr
type SFunction = [SExpr] -> SExpr 

showSExpr :: SExpr -> String
showSExpr (Atom s) = "'" ++ s
showSExpr (String s) = "\"" ++ s ++ "\""
showSExpr (Boolean b) = if b then "true" else "false"
showSExpr (Number f) = show f
showSExpr (List xs) = "[" ++  
							intercalate ", " (map showSExpr xs) 
						++ "]"
showSExpr (Sequence xs) = "{" ++  
							intercalate "\n" (map showSExpr xs) 
						++ "}"
showSExpr (BindFunc funcName args expr)  = "Function " ++ funcName ++ " " ++ (unwords $ map show args) ++ " " ++ (show expr)
showSExpr (Func _) =  "<<Generic Function>>" --s ++ " " ++  
							--intercalate " " (map showSExpr args) 

showSExpr (UserFunc args body) = "<<UFUNC " ++ (show args) ++ " " ++ (show body) ++ ">>" 
showSExpr (ExecFunc funcName args) = "<<EXEC FUNC " ++ (show funcName) ++ " " ++ (show args)++">>"  --"<<Function to be executed: " ++ funcName ++ ", probable error>>"
showSExpr (IOFunc _) = "<<Generic IO Function>>"
showSExpr (If cond _ _) = "If (" ++ (show cond) ++ ") ..."
showSExpr (Handle h) = "<<Handle " ++ (show h) ++ ">>"
showSExpr (BindLet _ _) = "<<Unevaled Let binding>>"
showSExpr (Error "") = ""
showSExpr (Error s) = "Error: " ++ s
showSExpr (Empty) = "Empty"











instance Show SExpr where
	show = showSExpr



instance Eq SExpr where
	(Number a) == (Number b) = a == b
	(String a) == (String b) = a == b
	(Boolean a) == (Boolean b) = a == b
	(List xs) == (List ys) = and $ map (\(a,b) -> a==b) $ zip xs ys
	(Empty) == (Empty) = True
	(_) == (_) = False