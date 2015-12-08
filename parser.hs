module Parser(parseExpr, parseMain, parseSequenceNoBrackets, parseWhile, parseListGen, parseUserFunc, parseFunc) where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.ParserCombinators.Parsec as P(spaces)
import Types

spaces :: Parser ()
spaces = skipMany1 (oneOf " ,")

symbol = oneOf "!@#$%^&*-+/<>=:"
uSymbols = oneOf "!@#$%^&*-+/<>"


functionSeparators = skipMany1 (oneOf " ")
listSeparators = skipMany1 (oneOf ",;")
sequenceSeparators = skipMany1 (oneOf " \n;")
--sequenceSkips = skipMany (oneOf "\n ")

whiteskips = skipMany (oneOf " \n\t")
-- separator = string ", "

parseNumber :: Parser SExpr
parseNumber = do
	neg <- char '-' <|> (return ' ')
	x <- many1 digit
	d <- (char '.') <|> (return ' ')
	y <- (many1 digit) <|> (return "")
	return $ Number $ read $ 
		if d == ' ' 
			then (neg:x)
			else (neg:x) ++ "." ++ y
	--return $ Number $ read x



parseString :: Parser SExpr
parseString = do
	char '"'
	x <- (many $ noneOf "\"")
	char '"'
	return $ String x



parseInfixFunc :: Parser SExpr
parseInfixFunc = do
	arg1 <- (parseFuncArgs (fail "noparams"))
	char ' ' <|> (return ' ')
	funcName <- many1 symbol
	char ' ' <|> (return ' ')
	arg2 <- (parseFuncArgs (fail "noparams"))
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
	args <- sepBy1 (parseFuncArgs (fail "noparams")) functionSeparators -- Previously was sepBy
	let args' = removeEmpties args
	--error $ "poes; " ++ (show $ length args') ++ "; " -- ++ (show args)
	if args' == [] 
		then fail "noargs"
		else return $ ExecFunc funcName args'



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
	string "then"
	conseq <- parseExpr
	whiteskips
	string "else"
	alter <- parseExpr
	return $ If cond conseq alter


parseWhile :: Parser SExpr
parseWhile = do
	string "while"
	cond <- parseExpr
	whiteskips
	string "do"
	body <- parseExpr
	whiteskips
	string "where"
	whiteskips
	varName <- many letter
	whiteskips
	char '='
	whiteskips
	initial <- parseExpr
	return $ WhileDo  varName initial cond  body

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
	let res = x ++ (removeEmpties [y])
	--sequenceSkips <|> (return ())
	whiteskips
	return res

parseComment :: Parser ()
parseComment = do
	string "--"
	many (noneOf "\n")
	return ()


parseNothing :: Parser SExpr
parseNothing = do
	string ""
	return $ Empty


parseListGen :: Parser SExpr
parseListGen = do
	char '['
	start <- parseExpr
	whiteskips
	string ".."
	whiteskips
	end <- parseExpr <|> (return Empty)
	whiteskips
	char ';' <|> (return ' ')
	inc <- parseExpr <|> (return $ Number 1)
	char ']'
	return $ BindListGen start end inc



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
removeEmpties = filter (\x -> x /= Empty)	 

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
	parseFuncArgs (try parseFunc)



parseFuncArgs' add = do
	(try parseInfixFunc) 
	<|>	parseFuncArgs (try add)

	
parseFuncArgs add = do
	P.spaces

	(try parseString)
	<|> (try parseNumber)
	<|> (try parseListGen)
	<|> (try parseList)
	<|> (try parseSequence)


	<|> (try parseUserFunc)		
	<|> (try parseIf)
	<|> (try parseWhile)

	<|> add
	<|> parseAtom'
--	<|> (try parseNothing)

