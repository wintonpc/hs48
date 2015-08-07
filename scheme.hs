module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

main :: IO ()
main  = do (expr:_) <- getArgs
           putStrLn (readExpr expr)

symbol :: Parser Char
symbol  = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces  = skipMany1 space


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
               deriving Show
               
 

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return (String x)

plainChar :: Parser Char
plainChar = noneOf "\""

escapedChar :: Parser Char
escapedChar = do (oneOf "\\")
                 c <- anyChar
                 return $ case c of
                   '\\' -> '\\'
                   '"' -> '"'
                   'n' -> '\n'
                   't' -> '\t'
                   _ -> error ("Unknown escape char: " ++ [c])

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do s <- many1 digit
                 return (Number (read s))

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
