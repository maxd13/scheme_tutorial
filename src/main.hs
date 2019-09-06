module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.Environment

concatenate [] = ""
concatenate (cs:css) = cs ++ " " ++ concatenate css

symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
--spaces = skipMany1 space
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of 
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _ -> Atom atom

parseNumber = liftM (Number . read) $ many1 digit

parseExpr = parseAtom <|> parseNumber <|> parseString

readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)