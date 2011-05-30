module Read
  where

import Control.Concurrent.STM
import Control.Monad
import Data.List as L
import Data.Map as M
import Data.Sequence as S
import Data.Traversable as T
import System
import Text.ParserCombinators.Parsec as Parse hiding (spaces)

import Env
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
               let op = (name "bard.prim" "make-sequence")
               return (Value.append (Value.makeSequence [op]) x)
        <|> do Parse.char '{'
               x <- parseSequence
               Parse.char '}'
               let op = (name "bard.prim" "make-map")
               return (Value.append (Value.makeSequence [op]) x)

readExpr :: String -> Env -> ModuleManager -> STM BardValue
readExpr input env mmgr = do
  case parse parseExpr "bard" input of
    Left err -> return (text ("Invalid input:" ++ show err))
    Right val -> readVal val env mmgr 

readVal :: BardValue -> Env -> ModuleManager -> STM BardValue
readVal val env mmgr = do
    case val of 
      (BVName _) -> readName val env mmgr 
      (BVSequence (BSequence s)) -> do s' <- T.traverse (\e -> readVal e env mmgr) s
                                       return (BVSequence (BSequence s'))
      _ -> return val


readName :: BardValue -> Env -> ModuleManager -> STM BardValue
readName (BVName (BName mname vname)) env mmgr = do
  case mname of
    "" -> do currmname <- getCurrentModule mmgr
             let (BVText (BText mname)) = currmname
             intern vname mname mmgr
    "bard.keyword" -> return (name mname vname)
    _ -> intern vname mname mmgr
