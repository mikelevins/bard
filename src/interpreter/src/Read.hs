module Read
  where

import Control.Concurrent.STM
import Control.Monad
import Data.List as L
import Data.Map as M
import System
import Text.ParserCombinators.Parsec as Parse hiding (spaces)

import Module
import Name
import Value

-------------------------------------------------
-- Parsers
-------------------------------------------------

punctuation :: Parser Char
punctuation = oneOf "!#$%&|*+-?<=>?@^_~" 

dot :: Parser Char
dot = Parse.char '.'

colon :: Parser Char
colon = Parse.char ':'

spaces :: Parser ()
spaces = skipMany1 space

modulenamepart :: Parser Char
modulenamepart = oneOf "abcdefghijklmnopqrstuvwxyz.-_1234567890"

modulenameend :: Parser Char
modulenameend = Parse.char ':'

modulename :: Parser String
modulename = do start <-letter
                body <- many modulenamepart
                end <- modulenameend
                return (start:body)

parseCharacter :: Parser BardValue
parseCharacter = do Parse.char '\\'
                    x <- anyChar
                    return (Value.char x)
                 
parseString :: Parser BardValue
parseString = do Parse.char '"'
                 x <- many (noneOf "\"")
                 Parse.char '"'
                 return (text x)
                 
unqualifiedName :: Parser String
unqualifiedName = do first <- letter <|> punctuation
                     rest <- many (letter <|> digit <|> punctuation)
                     return( first:rest)

parseQualifiedName :: Parser BardValue
parseQualifiedName = do mname <- modulename 
                        nm <- unqualifiedName
                        return (name mname nm)
                 
parseKeyword :: Parser BardValue
parseKeyword = do colon
                  nm <- unqualifiedName
                  return (name "bard.keyword" nm)
                 
parseUnqualifiedName :: Parser BardValue
parseUnqualifiedName = do nm <- unqualifiedName 
                          case nm of
                            "undefined" -> return Value.undefined
                            "nothing" -> return nothing
                            "true" -> return (boolean True)
                            "false" -> return (boolean False)
                            _ -> return (name "" nm)
                 
parseName :: Parser BardValue
parseName = try parseKeyword 
        <|> try parseQualifiedName 
        <|> try parseUnqualifiedName

parseInteger :: Parser BardValue
parseInteger = liftM (int . read) $ many1 digit

parseFloat :: Parser BardValue
parseFloat = do m <- many digit
                Parse.char '.'
                n <- many digit
                return (float (read (m ++ "." ++ n)))

parseNumber :: Parser BardValue
parseNumber = do n <- try parseFloat  <|> parseInteger
                 return n

parseSequence :: Parser BardValue
parseSequence = liftM Value.makeSequence $ sepBy parseExpr spaces 

parseQuoted :: Parser BardValue
parseQuoted = do
  Parse.char '\''
  x <- parseExpr
  return (Value.makeSequence [(name "bard.core" "quote"), x])

parseExpr :: Parser BardValue
parseExpr = parseName
        <|> parseCharacter
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do Parse.char '('
               x <- parseSequence
               Parse.char ')'
               return x
        <|> do Parse.char '['
               x <- parseSequence
               Parse.char ']'
               let op = (name "bard.core" "sequence")
               return (Value.append (Value.makeSequence [op]) x)
        <|> do Parse.char '{'
               plist <- parseSequence
               Parse.char '}'
               let iop = (name "bard.core" "sequence")
               let oop = (name "bard.core" "sequence->map")
               return (Value.cons oop (Value.makeSequence [(Value.cons iop plist)]))

readExpr :: String -> ModuleManager -> STM BardValue
readExpr input mmgr = do
  case parse parseExpr "bard" input of
    Left err -> return (text ("Invalid input:" ++ show err))
    Right val -> case val of 
                   (BVName _) -> readName val mmgr 
                   _ -> return val

readName :: BardValue -> ModuleManager -> STM BardValue
readName (BVName (BName mname vname)) mmgr = do
  case mname of
    "" -> do currmname <- getCurrentModule mmgr
             let (BVText (BText mname)) = currmname
             intern vname mname mmgr
    "bard.keyword" -> return (name mname vname)
    _ -> intern vname mname mmgr
