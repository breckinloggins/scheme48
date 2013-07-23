-- Write yourself a scheme in 48 hours
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where

import Text.ParserCombinators.Parsec hiding (many, spaces, (<|>))
import System.Environment
import Control.Monad
import Control.Applicative
import Numeric
import Data.Char

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Char Char
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
	x <- many $ escapeCharacer <|> noneOf "\""
	char '"'
	return $ String x

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many $ letter <|> digit <|> symbol
	let atom = first:rest
	return $ Atom atom

fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((< base) . digitToInt) digitToInt

-- https://news.ycombinator.com/item?id=6091386
hashLiteralInfo :: [(Char, Parser LispVal)]
hashLiteralInfo = [
	('f', pure $ Bool False),
	('t', pure $ Bool True),
	('b', fmap (Number . toInteger . fromBase 2) (many $ oneOf "01")),
	('o', fmap (Number . toInteger . fromBase 8) (many $ oneOf "01234567")),
	('d', fmap (Number . toInteger . fromBase 10) (many $ oneOf "0123456789")),
	('h', fmap (Number . toInteger . fromBase 16) (many $ oneOf "0123456789abcdefABCDEF"))
	]
	
parseHashLiteral :: Parser LispVal
parseHashLiteral = do
	char '#'
	c <- oneOf $ map fst hashLiteralInfo
	case lookup c hashLiteralInfo of
		Just x -> x
		Nothing -> fail "Internal parse error: unregistered literal info"


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

	
parseExpr :: Parser LispVal
parseExpr = parseNumber
	<|> parseString
	<|> parseHashLiteral
	<|> parseAtom

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found value " ++ show val

main :: IO ()
main = do
	args <- getArgs
	case length args of
		0 -> getLine >>= \line -> putStrLn $ readExpr line
		_ -> mapM_ putStrLn $ map readExpr args

	