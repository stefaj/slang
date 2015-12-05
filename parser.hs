import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef
import Types
import Primitives
import Control.Monad.ST
import System.IO

spaces :: Parser ()
spaces = skipMany1 (oneOf " ,")

symbol = oneOf "!@#$%^&*-+=/"

functionSeparators = skipMany1 (oneOf " ")
listSeparators = skipMany1 (oneOf " ,;")
sequenceSeparators = skipMany1 (oneOf "\n; ")

-- separator = string ", "

parseNumber :: Parser SExpr
parseNumber = do
	x <- many1 digit
	d <- (char '.') <|> (return ' ')
	y <- (many1 digit) <|> (return "")
	return $ Number $ read $ 
		if d == ' ' 
			then x
			else x ++ "." ++ y
	--return $ Number $ read x



parseString :: Parser SExpr
parseString = do
	char '"'
	x <- (many $ noneOf "\"")
	char '"'
	return $ String x

parseFunc :: Parser SExpr
parseFunc = do
	-- first <- char '`'
	funcName <- many (letter <|> symbol)
	char ' '
	args <- sepBy parseExpr functionSeparators
	return $ ExecFunc funcName args

parseAtom :: Parser SExpr
parseAtom = do
	first <- char '`'
	first <- letter
	rest <- many (letter)
	let atom = first:rest
	return $ Atom atom

parseList :: Parser SExpr
parseList = do
	char '['
	x <- sepBy parseExpr listSeparators
	char ']'
	return $ List x


parseSequence :: Parser SExpr
parseSequence = do
	char '{'
	x <- sepBy parseExpr sequenceSeparators
	char '}'
	return $ Sequence x

parseUserFunc = do 
		string "func"
		char ' '
		funcName <- many letter
		char ' '
		args <- getArgs
		string "= "
		func <- parseExpr
		return $ BindFunc funcName (removeEmptyStrings args) func  
	 
removeEmptyStrings :: [String] -> [String]
removeEmptyStrings [] = []
removeEmptyStrings (x:xs)
	| (x==" ") || (x=="") = removeEmptyStrings xs
	| otherwise	= x:(removeEmptyStrings xs)

getArgs :: Parser [String]
getArgs = do
	sepBy getArg (skipMany1 (oneOf " "))

getArg :: Parser [Char]
getArg = do
	--x <- letter
	xs <- many (letter)
	--(char ' ') <|> (return ' ')
	return xs

parseExpr = do
	parseAtom
	<|> parseString
	<|> parseNumber
	<|> parseList
	<|> parseSequence
	<|> parseUserFunc
	<|> parseFunc

	
readExpr2 expr input = let o = parse expr "sl" input
					in case o of
						Left err -> "Error" ++ (show err)
						Right val -> show $ val


readExpr' :: (Parser SExpr) -> String -> SExpr
readExpr' expr input = let o = parse expr "sl" input
					in case o of
						Left err -> Error $ show err
						Right val -> val

readExpr input = readExpr' parseExpr input

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action



flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
						sexpr <- evalString env expr
						putStrLn sexpr

--evalString :: Env -> String -> IO String

evalString :: Env -> String -> IO String
evalString env expr = do 
				e <- (eval env (readExpr expr))
				return $ show e

runRepl :: IO ()
runRepl = basicBindingsEnv >>= until_ (== "quit") (readPrompt "SL > ") . evalAndPrint					

main :: IO ()
main = do
	runRepl
	--until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint 

