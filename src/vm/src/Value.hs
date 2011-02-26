module Value
    where

import Data.List as L
import Data.Map as M
import Data.Sequence as S

-------------------------------------------------
-- Values
-------------------------------------------------

data BardValue = BardUndefined
           | BardNothing
           | BardBoolean Bool
           | BardInteger Integer
           | BardFloat Double
           | BardCharacter Char
           | BardName String
           | BardSequence (S.Seq BardValue)
           | BardMap (M.Map BardValue BardValue)
             deriving (Eq, Ord)

-- constructors

nothing = BardNothing
bool b = BardBoolean b
int n = BardInteger n
float f = BardFloat f
char ch = BardCharacter ch
name nm = BardName nm

emptyMap = BardMap M.empty

map :: [(BardValue,BardValue)] -> BardValue
map [] = BardMap M.empty
map vals = BardMap (M.fromList vals)

emptySequence = BardSequence S.empty

sequence :: [BardValue] -> BardValue
sequence [] = BardSequence S.empty
sequence vals = BardSequence (S.fromList vals)

-- accessors

first (BardSequence s) = S.index s 0
second (BardSequence s) = S.index s 1
third (BardSequence s) = S.index s 2

get (BardMap m) k = M.lookup k m

-- printers

instance Show BardValue where
    show BardUndefined = "undefined"
    show BardNothing = "nothing"
    show (BardBoolean True) = "true"
    show (BardBoolean False) = "false"
    show (BardInteger n) = (show n)
    show (BardFloat f) = (show f)
    show (BardCharacter ch) = ['\\',ch]
    show (BardName s) = s
    show (BardMap m) = "{" ++ (expand m) ++ "}"
        where expand m = concat (L.intersperse " " (Prelude.map showPair (M.assocs m)))
              showPair (k, v) = (show k) ++ " " ++ (show v)
    show (BardSequence s) = "(" ++ (expand s) ++ ")"
        where expand s = concat (L.intersperse " " (Prelude.map showNth (Prelude.take (S.length s) [0 ..])))
              showNth i = (show (index s i))

