module Parser(parseExpr, parseMain, parseSequenceNoBrackets) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Types

spaces :: Parser ()
spaces = skipMany1 (oneOf " ,")

symbol = oneOf "!@#$%^&*-+=/"

functionSeparators = skipMany1 (oneOf " ")
listSeparators = skipMany1 (oneOf " ,;")
sequenceSeparators = skipMany1 (oneOf "\n; ")
sequenceSkips = skipMany (oneOf "\n ")
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
	seqi <- parseSequenceNoBrackets
	char '}'
	return seqi

parseSequenceNoBrackets :: Parser SExpr -- Mainly used by runeOnce to parse a main file
parseSequenceNoBrackets = do
	--sequenceSkips <|> (return ())
	x <- sepBy parseExpr sequenceSeparators
	--sequenceSkips <|> (return ())
	return $ Sequence x

parseMain :: Parser [SExpr] -- Mainly used by runeOnce to parse a main file
parseMain = do
	sequenceSkips <|> (return ())
	x <- endBy parseExpr sequenceSeparators
	sequenceSkips <|> (return ())
	return x



parseUserFunc = do 
		string "func"
		char ' '
		funcName <- many letter
		char ' '
		args <- getArgs'
		string "= "
		func <- parseExpr
		return $ BindFunc funcName (removeEmptyStrings args) func  
	 
removeEmptyStrings :: [String] -> [String]
removeEmptyStrings [] = []
removeEmptyStrings (x:xs)
	| (x==" ") || (x=="") = removeEmptyStrings xs
	| otherwise	= x:(removeEmptyStrings xs)

getArgs' :: Parser [String]
getArgs' = do
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

	


	--until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint 

