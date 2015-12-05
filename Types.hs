module Types(NumericType, SExpr(..), SFunction) where

import Data.List
-- to easily change it later if necessary
type NumericType = Double

data SExpr = Atom String
		| Number NumericType
		| String String
		| Bool Bool
		| List [SExpr] 
		| Sequence [SExpr]
--		| Func String [SExpr]
		| ExecFunc String [SExpr]
		| BindFunc String [SExpr] SExpr
		| Func ([SExpr] -> SExpr)
		| UserFunc String [SExpr] SExpr
		| Error String



-- maps function with parameters to a SExpr
type SFunction = [SExpr] -> SExpr 

showSExpr :: SExpr -> String
showSExpr (Atom s) = "'" ++ s
showSExpr (String s) = "\"" ++ s ++ "\""
showSExpr (Bool b) = if b then "true" else "false"
showSExpr (Number f) = show f
showSExpr (List xs) = "[" ++  
							intercalate ", " (map showSExpr xs) 
						++ "]"
showSExpr (Sequence xs) = "{" ++  
							intercalate "\n" (map showSExpr xs) 
						++ "}"

--showSExpr (Func s args) = s ++ " " ++  
--							intercalate " " (map showSExpr args) 
showSExpr (Error s) = "Error: " ++ s
--showSExpr (UserFunc name args body) = "uf: " ++ name ++ " " ++ (unwords args)

instance Show SExpr where
	show = showSExpr