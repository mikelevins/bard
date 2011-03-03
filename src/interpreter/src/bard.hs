module Main
  where

import Control.Monad
import Data.List as L
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import BardValue

-------------------------------------------------
-- Parsers
-------------------------------------------------

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
               plist <- parseSequence
               char '}'
               let iop = (BardName "sequence")
               let oop = (BardName "sequence->map")
               return (BardValue.cons oop (BardValue.sequence [(BardValue.cons iop plist)]))

readExpr :: String -> String
readExpr input = case parse parseExpr "bard" input of
  Left err -> "No match:" ++ show err
  Right val -> "Found value: " ++ (show val)

-------------------------------------------------
-- evaluator
-------------------------------------------------

eval :: BardValue -> BardValue 
eval BardUndefined = BardUndefined
eval BardNothing = BardNothing
eval val@(BardBoolean _) = val
eval val@(BardInteger _) = val
eval val@(BardFloat _) = val
eval val@(BardCharacter _) = val
eval val@(BardText _) = val

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard v 1.0"
       putStrLn ""
       args <- getArgs
       putStrLn (readExpr (args !! 0))

