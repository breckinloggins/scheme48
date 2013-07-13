-- Write yourself a scheme in 48 hours
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Bool Bool
	deriving Show

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapeCharacer :: Parser Char
escapeCharacer = do
	char '\\'
	c <- anyChar
	case c of
		'"' -> return '"'
		'n' -> return '\n'
		'r' -> return '\r'
		't' -> return '\t'
		'\\' -> return '\\'
		_ -> fail "Unrecognized escape sequence"

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many (escapeCharacer <|> noneOf "\"")
	char '"'
	return $ String x

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
	
parseExpr :: Parser LispVal
parseExpr = parseAtom
	<|> parseString
	<|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found value " ++ show val

main :: IO ()
main = do
	args <- getArgs
	code <- case length args of
		0 -> getLine
		_ -> return $ (args !! 0)

	putStrLn (readExpr code)
	