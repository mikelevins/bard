module Read
  where

import Control.Concurrent.MVar
import Control.Monad
import Data.List as L
import Data.Map as M
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import Value

-------------------------------------------------
-- Parsers
-------------------------------------------------

punctuation :: Parser Char
punctuation = oneOf "!#$%&|*+_?<=>?@^_~" 

dot :: Parser Char
dot = char '.'

colon :: Parser Char
colon = char ':'

spaces :: Parser ()
spaces = skipMany1 space

modulenamepart :: Parser Char
modulenamepart = oneOf "abcdefghijklmnopqrstuvwxyz.-_1234567890"

modulenameend :: Parser Char
modulenameend = char ':'

modulename :: Parser String
modulename = do start <-letter
                body <- many modulenamepart
                end <- modulenameend
                return (start:body)

parseCharacter :: Parser BardValue
parseCharacter = do char '\\'
                    x <- anyChar
                    return (BardCharacter x)
                 
parseString :: Parser BardValue
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return (BardText x)
                 
unqualifiedName :: Parser String
unqualifiedName = do first <- letter <|> punctuation
                     rest <- many (letter <|> digit <|> punctuation)
                     return( first:rest)

parseQualifiedName :: Parser BardValue
parseQualifiedName = do mname <- modulename 
                        name <- unqualifiedName
                        return (BardName name)
                 
parseKeyword :: Parser BardValue
parseKeyword = do colon
                  nm <- unqualifiedName
                  return (BardName nm)
                 
parseUnqualifiedName :: Parser BardValue
parseUnqualifiedName = do name <- unqualifiedName 
                          case name of
                            "undefined" -> return BardUndefined
                            "nothing" -> return BardNothing
                            "true" -> return (BardBoolean True)
                            "false" -> return (BardBoolean False)
                            _ -> return (BardName name)
                 
parseName :: Parser BardValue
parseName = try parseKeyword 
        <|> try parseQualifiedName 
        <|> try parseUnqualifiedName

parseInteger :: Parser BardValue
parseInteger = liftM (BardInteger . read) $ many1 digit

parseFloat :: Parser BardValue
parseFloat = do m <- many digit
                char '.'
                n <- many digit
                return (BardFloat (read (m ++ "." ++ n)))

parseNumber :: Parser BardValue
parseNumber = do n <- try parseFloat  <|> parseInteger
                 return n

parseSequence :: Parser BardValue
parseSequence = liftM Value.sequence $ sepBy parseExpr spaces 

parseQuoted :: Parser BardValue
parseQuoted = do
  char '\''
  x <- parseExpr
  return (Value.sequence [(BardName "quote"), x])

parseExpr :: Parser BardValue
parseExpr = parseName
        <|> parseCharacter
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- parseSequence
               char ')'
               return x
        <|> do char '['
               x <- parseSequence
               char ']'
               let op = (BardName "sequence")
               return (Value.append (Value.sequence [op]) x)
        <|> do char '{'
               plist <- parseSequence
               char '}'
               let iop = (BardName "sequence")
               let oop = (BardName "sequence->map")
               return (Value.cons oop (Value.sequence [(Value.cons iop plist)]))

readExpr :: String -> BardValue
readExpr input = case parse parseExpr "bard" input of
  Left err -> BardText ("No match:" ++ show err)
  Right val -> val

