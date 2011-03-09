module Read
  where

import Control.Monad
import Data.List as L
import Data.Functor as F
import Data.Map as M
import Data.Sequence as S
import Data.Traversable as T
import System
import Text.ParserCombinators.Parsec as Parse hiding (spaces)

import Runtime
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
parseSequence = liftM Value.makeSequence $ endBy parseExpr spaces 

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

readExpr :: String -> BardRuntime -> (BardValue, BardRuntime)
readExpr input bard =
  case parse parseExpr "bard" input of
    Left err -> ((text ("Invalid input:" ++ show err)), bard)
    Right val -> 
        case val of
          (BVName _) -> intern val bard
          (BVSequence (BSequence s)) -> let (_,s',bard') = readSequence s (S.empty) bard
                                        in ((BVSequence (BSequence s')),bard')
          _ -> (val, bard) 

intern :: BardValue -> BardRuntime -> (BardValue, BardRuntime)
intern val@(BVName (BName mname vname)) bard = 
    case mname of
      "" -> let mname = (getCurrentModuleName bard)
            in ((BVName (BName mname vname)),bard)
      "bard.keyword" -> (val, bard)
      _ -> (val,bard)

intern val bard = (val,bard)

readSequence inseq outseq bard =
    if (S.null inseq) 
       then (inseq,outseq,bard)
       else let inseq' = S.drop 1 inseq
                item = S.index inseq 0
                (item',bard') = intern item bard
                outseq' = outseq |> item'
            in readSequence inseq' outseq' bard'