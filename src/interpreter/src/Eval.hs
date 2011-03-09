module Eval
    where

import Data.Foldable as F
import Data.List as L
import Data.Map as M
import Data.Sequence as S
import Data.Traversable as T

import Runtime
import Value

---------------------------------------------------------------------
-- eval
---------------------------------------------------------------------

eval :: BardValue -> BardRuntime -> (BardValue,BardRuntime)

-- self-evaluating
eval val@(BVUndefined _) bard = (val,bard)
eval val@(BVNothing _) bard =  (val,bard)
eval val@(BVBoolean _) bard =  (val,bard)
eval val@(BVInteger _) bard =  (val,bard)
eval val@(BVFloat _) bard =  (val,bard)
eval val@(BVCharacter _) bard =  (val,bard)
eval val@(BVText _) bard =  (val,bard)
eval val@(BVMap _) bard =  (val,bard)

---------------------------------------------------------------------
-- apply
---------------------------------------------------------------------

