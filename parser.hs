module Parser(parseExpr, parseMain, parseSequenceNoBrackets, parseIf, parseInfixFunc) where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec as P(spaces)
import Types

spaces :: Parser ()
spaces = skipMany1 (oneOf " ,")

symbol = oneOf "!@#$%^&*-+/<>="
uSymbols = oneOf "!@#$%^&*-+/<>"


functionSeparators = skipMany1 (oneOf " ")
listSeparators = skipMany1 (oneOf " ,;")
sequenceSeparators = skipMany1 (oneOf "\n; ")
--sequenceSkips = skipMany (oneOf "\n ")

whiteskips = skipMany (oneOf " \n\t")
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



parseInfixFunc :: Parser SExpr
parseInfixFunc = do
	arg1 <- parseExpr'
	char ' ' <|> (return ' ')
	funcName <- many1 symbol
	char ' ' <|> (return ' ')
	arg2 <- parseExpr'
	return $ ExecFunc funcName [arg1, arg2]

parseAtom :: Parser SExpr
parseAtom = do
	funcName <- many letter
	case funcName of
		"true" -> return $ Boolean True
		"false" -> return $ Boolean False
		otherwise -> return $ if (length funcName) > 0
			then Atom funcName
			else Error ""

parseFunc :: Parser SExpr
parseFunc = do


	funcName <- many (letter <|> symbol)
	char ' '
	char '('
	args <- sepBy1 parseExpr functionSeparators -- Previously was sepBy
	char ')'
	-- c <- char ' ' <|> (return 'a')
	return $ ExecFunc funcName args
	

--	if (c == 'a') 
--		then return $ Atom funcName
--		else
--			do
--				args <- sepBy1 parseExpr functionSeparators -- Previously was sepBy
--				return $ ExecFunc funcName args




parseAtom' :: Parser SExpr
parseAtom' = do
	--char '`'
	first <- letter
	rest <- many (letter)
	let atom = first:rest
	case atom of
		"true" -> return $ Boolean True
		"false" -> return $ Boolean False
		"let" -> fail "wrong turn"
		otherwise -> return $ Atom atom

parseList :: Parser SExpr
parseList = do
	char '['
	x <- sepBy parseExpr listSeparators
	char ']'
	return $ List x

parseIf :: Parser SExpr
parseIf = do
	string "if"
	cond <- parseExpr
	whiteskips
	--char ' ' <|> (return ' ')
	string "then"
	-- char ' ' <|> (return ' ')
	conseq <- parseExpr
	--char ' ' <|> (return ' ')
	whiteskips
	string "else"
	-- char ' ' <|> (return ' ')
	alter <- parseExpr
	return $ If cond conseq alter

parseSequence :: Parser SExpr
parseSequence = do
	char '{'
	whiteskips
	seqi <- parseSequenceNoBrackets
	whiteskips
	char '}'
	return seqi

parseSequenceNoBrackets :: Parser SExpr -- Mainly used by runeOnce to parse a main file
parseSequenceNoBrackets = do
	--sequenceSkips <|> (return ())
	x <- endBy parseExpr sequenceSeparators
	--sequenceSkips <|> (return ())
	return $ Sequence x

parseMain :: Parser [SExpr] -- Mainly used by runeOnce to parse a main file
parseMain = do
	whiteskips
	--sequenceSkips <|> (return ())
	x <- (try $ endBy parseExpr sequenceSeparators) <|> (return [])
	y <- parseExpr <|> (return Empty)
	let res = x ++ (filter (\x -> x /= Empty) [y])
	--sequenceSkips <|> (return ())
	whiteskips
	return res

parseComment :: Parser ()
parseComment = do
	string "--"
	many (noneOf "\n")
	return ()


parseUserFunc :: Parser SExpr
parseUserFunc = do 
		string "let"
		char ' '
		funcName <- many (letter <|> uSymbols)
		char ' '
		args <- getArgs'
		string "="
		char ' ' <|> (return ' ')
		func <- parseExpr
		let args' = (removeEmptyStrings args)
		return $ case (length args') of
					0 -> BindLet funcName func
					otherwise -> BindFunc funcName args' func  

--parseLet :: Parser SExpr
--parseLet = do 
--		string "let"
--		char ' '
--		funcName <- many letter
--		string " = "		
--		func <- parseExpr
--		return $ BindLet funcName func  	
	 
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
	P.spaces
	--skipMany (oneOf " ")
	a <- do (	(try parseInfixFunc)
				<|> parseExpr')
	--P.spaces
	return a

parseExpr' = do

	P.spaces
	--(try parseComment) <|>
	

	--skipMany (oneOf " ")
	a <- do (	
				parseString
				<|> parseNumber
				<|> parseList
				<|> parseSequence
			--	<|> parseLet

				<|> (try parseUserFunc)		
				<|> (try parseIf)
				<|> (try parseFunc)
				<|> parseAtom'
							--	<|> (try parseAtom)	

				)
	--P.spaces
	return a

	


	--until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint 

