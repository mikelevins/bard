module Main
  where

import Control.Monad
import Data.List
import System
import Text.ParserCombinators.Parsec hiding (spaces)

-------------------------------------------------
-- expressions
-------------------------------------------------

type ModuleName = [String]

data Expression = NameExp String
                | SequenceExp [Expression]
                | SequenceConstructorExp [Expression]
                | MapConstructorExp [Expression]
                | IntegerExp Integer
                | FloatExp Float
                | TextExp String deriving Show

-------------------------------------------------
-- reader
-------------------------------------------------

minusSign :: Parser Char
minusSign = char '-'

punctuation :: Parser Char
punctuation = oneOf "!$%&|*+-/<=>?@^_~"

colon :: Parser Char
colon = char ':'

dot :: Parser Char
dot = char '.'

spaces :: Parser ()
spaces = skipMany1 space

parseInteger :: Parser Expression
parseInteger = do maybeSign <- (optionMaybe minusSign)
                  let sign = (case maybeSign of
                                Just s -> -1
                                Nothing -> 1)
                  x <- (many1 digit)
                  let i = (IntegerExp . (* sign) . read) x
                  return i

parseFloat :: Parser Expression
parseFloat = do maybeSign <- (optionMaybe minusSign)
                let sign = (case maybeSign of
                              Just s -> -1
                              Nothing -> 1)
                x <- (many1 digit)
                d <- char '.'
                y <- (many1 digit)
                let fs = (x ++ [d] ++ y)
                    f = (FloatExp . (* sign) . read) fs
                return f

parseName :: Parser Expression
parseName = do first <- (letter <|> punctuation <|> colon <|> dot)
               rest <- many (letter <|> punctuation <|> colon <|> dot <|> digit)
               let name = first:rest
               return $ NameExp name

parseText :: Parser Expression
parseText = do char '"'
               x <- many (noneOf "\"")
               char '"'
               return $ TextExp x

parseSequence :: Parser Expression
parseSequence = liftM SequenceExp (sepBy parseExpression spaces)

parseSequenceConstructor :: Parser Expression
parseSequenceConstructor = liftM SequenceConstructorExp (sepBy parseExpression spaces)

parseMapConstructor :: Parser Expression
parseMapConstructor = liftM MapConstructorExp (sepBy parseExpression spaces)

parseExpression :: Parser Expression
parseExpression =  parseText
                  <|> do char '('
                         skipMany spaces
                         x <- parseSequence
                         char ')'
                         return x
                  <|> do char '['
                         skipMany spaces
                         x <- parseSequenceConstructor
                         char ']'
                         return x
                  <|> do char '{'
                         skipMany spaces
                         x <- parseMapConstructor
                         char '}'
                         return x
                  <|> do x <- try parseFloat <|> parseInteger
                         return x
                  <|> do parseName

readExpr :: String -> String
readExpr input = case parse parseExpression "bard" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

-------------------------------------------------
-- main
-------------------------------------------------

main :: IO ()
main = do args <- getArgs
          putStrLn "bardc v 1.0"
          putStrLn (readExpr (args !! 0))
