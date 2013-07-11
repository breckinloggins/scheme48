-- Write yourself a scheme in 48 hours
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where
import System.Environment

main :: IO ()
main = do
	putStrLn "Please type your name"
	name <- getLine
	args <- getArgs
	putStrLn (name ++ ", sum is: " ++ show (sum (map read args :: [Int])))

