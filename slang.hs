import Data.IORef
import Types
import Primitives
import Parser
import Control.Monad.ST
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified System.Environment as E

readExpr2 expr input = let o = parse expr "sl" input
					in case o of
						Left err -> "Error" ++ (show err)
						Right val -> show $ val



readExpr' :: (Parser SExpr) -> String -> SExpr
readExpr' expr input = let o = parse expr "sl" input
					in case o of
						Left err -> Error $ show err
						Right val -> val

readExpr = readExpr' parseExpr


isValidExpr expr input = let o = parse expr "sl" input
					in case o of
						Left err -> False
						Right val -> True





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

evalAndPrint' :: Env -> String -> Parser SExpr -> IO ()
evalAndPrint' env expr parseType = do
						sexpr <- evalString' env expr parseType
						if (sexpr == "")
							then putStr ""
							else putStrLn sexpr

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalAndPrint' env expr parseExpr
--evalString :: Env -> String -> IO String



evalString' :: Env -> String -> Parser SExpr -> IO String
evalString' env expr parseType = do 
				e <- (eval env (readExpr' parseType expr))
				return $ show e

evalString :: Env -> String -> IO String
evalString env expr = evalString' env expr parseExpr 


runRepl :: IO ()
runRepl = basicBindingsEnv >>= until_ (== "quit") (readPrompt "SL > ") . evalAndPrint					

runOnce :: String -> IO ()
runOnce contents = do
	basicEnv <- basicBindingsEnv
	evalAndPrint' basicEnv contents parseSequenceNoBrackets


main :: IO ()
main = do
	args <- E.getArgs
	case length args of 
		0 -> runRepl
		otherwise -> readFile (args !! 0) >>= runOnce






-- tests
-- parsing
test 1 = isValidExpr parseExpr "{ 1 }"
test 2 = isValidExpr parseExpr "{ + (1 1) }"
test 3 = isValidExpr parseExpr "+ 1 1"
test 4 = isValidExpr parseExpr "func fib a b = 1"
test 5 = isValidExpr parseExpr "let a = 10"
test 6 = isValidExpr parseExpr "let fib a b = + `a `b"

testAll = and $ map test [1..6]