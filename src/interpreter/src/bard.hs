module Main
  where

import Control.Monad
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import BardValue

punctuation :: Parser Char
punctuation = oneOf "!#$%&|*+_?:<=>?@^_~" 

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser BardValue
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return (BardText x)
                 
parseName :: Parser BardValue
parseName = do first <- letter <|> punctuation
               rest <- many (letter <|> digit <|> punctuation)
               let nm = first:rest
               case nm of
                 "undefined" -> return BardUndefined
                 "nothing" -> return BardNothing
                 "true" -> return (BardBoolean True)
                 "false" -> return (BardBoolean False)
                 _ -> return (BardName nm)
                 
parseInteger :: Parser BardValue
parseInteger = liftM (BardInteger . read) $ many1 digit

parseSequence :: Parser BardValue
parseSequence = liftM BardValue.sequence $ sepBy parseExpr spaces 

chunkByTwo :: [BardValue] -> [(BardValue,BardValue)]
chunkByTwo [] = []
chunkByTwo (x:y:vals) = [(x,y)] ++ (chunkByTwo vals)

parseMap :: Parser BardValue
parseMap = do items <- (sepBy parseExpr spaces)
              let pairs = (chunkByTwo items)
              return (BardValue.map pairs)
              
parseQuoted :: Parser BardValue
parseQuoted = do
  char '\''
  x <- parseExpr
  return (BardValue.sequence [BardName "quote", x])

parseExpr :: Parser BardValue
parseExpr = parseName
        <|> parseString
        <|> parseInteger
        <|> parseQuoted
        <|> do char '('
               x <- parseSequence
               char ')'
               return x
        <|> do char '['
               x <- parseSequence
               char ']'
               let op = (BardName "sequence")
               return (BardValue.append (BardValue.sequence [op]) x)
        <|> do char '{'
               x <- parseMap
               char '}'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "bard" input of
  Left err -> "No match:" ++ show err
  Right val -> "Found value: " ++ (show val)

-------------------------------------------------
-- 
-------------------------------------------------

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard v 1.0"
       putStrLn ""
       args <- getArgs
       putStrLn (readExpr (args !! 0))

