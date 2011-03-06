module Value
    where

import Control.Concurrent.STM
import Control.Monad.STM
import Data.Foldable as F
import Data.List as L
import Data.Map as M
import Data.Sequence as S

import Name

-------------------------------------------------
-- Values
-------------------------------------------------

data BardValue = BardUndefined
               | BardNothing
               | BardBoolean Bool
               | BardInteger Integer
               | BardFloat Double
               | BardCharacter Char
               | BardText String
               | BardName Name
               | BardBox Box
               | BardSequence (S.Seq BardValue)
               | BardMap (M.Map BardValue BardValue)
                 deriving (Eq, Ord)

type Box = TVar BardValue

-- make it possible to derive Ord for Box
instance Ord a => Ord (TVar a)

-- constructors

nothing = BardNothing
bool b = BardBoolean b
int n = BardInteger n
float f = BardFloat f
name nm = BardName nm

emptyMap = BardMap M.empty

map :: [(BardValue,BardValue)] -> BardValue
map [] = BardMap M.empty
map vals = BardMap (M.fromList vals)

emptySequence = BardSequence S.empty

sequence :: [BardValue] -> BardValue
sequence [] = BardSequence S.empty
sequence vals = BardSequence (S.fromList vals)

append :: BardValue -> BardValue -> BardValue
append (BardSequence s1) (BardSequence s2) = (BardSequence (s1 >< s2))
append _ _ = BardUndefined

cons :: BardValue -> BardValue -> BardValue
cons val (BardSequence s2) = (BardSequence (val <| s2))
cons _ _ = BardUndefined

-- accessors

getKey (BardMap m) k = M.lookup k m

first (BardSequence s) = S.index s 0
second (BardSequence s) = S.index s 1
third (BardSequence s) = S.index s 2



