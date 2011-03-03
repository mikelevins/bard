module Main
  where

import Control.Monad
import Data.List as L
import System
import Text.ParserCombinators.Parsec hiding (spaces)

import BardValue
import Reader

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

