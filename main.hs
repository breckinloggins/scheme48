-- Write yourself a scheme in 48 hours
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char

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

fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((<base).digitToInt) digitToInt

hashLiteralInfo = [
	('f', (Nothing, \_ -> Bool False)),
	('t', (Nothing, \_ -> Bool True)),
	('b', (Just "01", Number . toInteger . fromBase 2)),
	('o', (Just "01234567", Number . toInteger . fromBase 8)),
	('d', (Just "0123456789", Number . toInteger . fromBase 10)),
	('h', (Just "0123456789abcdefABCDEF", Number . toInteger . fromBase 16))
	]

parseHashLiteral :: Parser LispVal
parseHashLiteral = do
	char '#'
	c <- oneOf (map fst hashLiteralInfo)
	let literalInfo = lookup c hashLiteralInfo
	case literalInfo of
		Nothing -> fail "Internal parse error: unregistered literal info"
		Just (Nothing, f) -> return $ f "unneeded" -- I know there's a more idiomatic way to do that
		Just (Just validChars, f) -> do
			digits <- many (oneOf validChars)
			return $ f digits
		
parseNumber :: Parser LispVal
parseNumber = parseHashLiteral
	<|> (liftM (Number . read) $ many1 digit)

	
parseExpr :: Parser LispVal
parseExpr = parseNumber
	<|> parseString
	<|> parseAtom

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
	