module Reader
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
                        return (BardName mname name)
                 
parseKeyword :: Parser BardValue
parseKeyword = do colon
                  nm <- unqualifiedName
                  return (BardName "bard.keyword" nm)
                 
parseUnqualifiedName :: Parser BardValue
parseUnqualifiedName = do name <- unqualifiedName 
                          case name of
                            "undefined" -> return BardUndefined
                            "nothing" -> return BardNothing
                            "true" -> return (BardBoolean True)
                            "false" -> return (BardBoolean False)
                            _ -> return (BardName "" name)
                 
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
parseSequence = liftM BardValue.sequence $ sepBy parseExpr spaces 

parseQuoted :: Parser BardValue
parseQuoted = do
  char '\''
  x <- parseExpr
  return (BardValue.sequence [(BardName "bard.lang" "quote"), x])

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
               let op = (BardName "bard.core" "sequence")
               return (BardValue.append (BardValue.sequence [op]) x)
        <|> do char '{'
               plist <- parseSequence
               char '}'
               let iop = (BardName "bard.core" "sequence")
               let oop = (BardName "bard.core" "sequence->map")
               return (BardValue.cons oop (BardValue.sequence [(BardValue.cons iop plist)]))

readExpr :: String -> BardValue
readExpr input = case parse parseExpr "bard" input of
  Left err -> BardText ("No match:" ++ show err)
  Right val -> val

