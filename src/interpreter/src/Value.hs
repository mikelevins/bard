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

type Box = TVar BardValue

-- make it possible to derive Ord for Box
instance Ord a => Ord (TVar a)

data BUndefined = BUndefined deriving (Eq, Ord)
data BNothing = BNothing deriving (Eq, Ord)
data BBoolean = BTrue
			  | BFalse deriving (Eq, Ord)
data BInteger = BInteger Integer deriving (Eq, Ord)
data BFloat = BFloat Double deriving (Eq, Ord)
data BCharacter = BCharacter Char deriving (Eq, Ord)
data BText = BText String deriving (Eq, Ord)
data BName = BName ModuleName VariableName deriving (Eq, Ord)
data BBox = BBox Box deriving (Eq, Ord)
data BSequence = BSequence (S.Seq BardValue) deriving (Eq, Ord)
data BMap = BMap (M.Map BardValue BardValue) deriving (Eq, Ord)

data BardValue = BVUndefined BUndefined
               | BVNothing BNothing
               | BVBoolean BBoolean
               | BVInteger BInteger
               | BVFloat BFloat
               | BVCharacter BCharacter
               | BVText BText
               | BVName BName
               | BVBox BBox
               | BVSequence BSequence
               | BVMap BMap
                 deriving (Eq, Ord)

-- constructors

undefined = BVUndefined BUndefined
nothing = BVNothing BNothing
boolean True = BVBoolean BTrue
boolean False = BVBoolean BFalse
int n = BVInteger (BInteger n)
float f = BVFloat (BFloat f)
char ch = BVCharacter (BCharacter ch)
text tx = BVText (BText tx)
name mnm nm = BVName (BName mnm nm)
box v = BVBox (BBox v)
makeSequence vs = BVSequence (BSequence (S.fromList vs))
makeMap inits =  BVMap (BMap (M.fromList inits))

cons bval (BVSequence (BSequence bseq)) = BVSequence (BSequence (bval <| bseq))
append (BVSequence (BSequence bseq1))
       (BVSequence (BSequence bseq2)) = BVSequence (BSequence (bseq1 >< bseq2))