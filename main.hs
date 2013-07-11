-- Write yourself a scheme in 48 hours
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	putStrLn ("SUM: " ++ show (sum (map read args :: [Int])))

