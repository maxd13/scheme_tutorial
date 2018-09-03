module Main where
import System.Environment

concatenate [] = ""
concatenate (cs:css) = cs ++ " " ++ concatenate css


main :: IO ()
main = do
    putStrLn ("What is your name?")
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")