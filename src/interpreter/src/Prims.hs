module Prims
    where

import Data.Foldable as F
import Data.Map as M
import Data.Sequence as S

import Value

----------------------------------------------------------------------
-- Prim constructor
----------------------------------------------------------------------

makePrim :: String -> ([BardValue] -> BardValue) -> BardValue
makePrim pname f = BVPrim (BPrim pname f)

----------------------------------------------------------------------
-- int+
----------------------------------------------------------------------

primIntAdd = makePrim "int+" primitiveIntAdd
primitiveIntAdd :: [BardValue] -> BardValue
primitiveIntAdd [(BVInteger (BInteger m)),(BVInteger (BInteger n))] = (int (m+n))

----------------------------------------------------------------------
-- float+
----------------------------------------------------------------------

primFloatAdd = makePrim "float+" primitiveFloatAdd
primitiveFloatAdd :: [BardValue] -> BardValue
primitiveFloatAdd [(BVFloat (BFloat m)),(BVFloat (BFloat n))] = (float (m+n))

----------------------------------------------------------------------
-- int*
----------------------------------------------------------------------

primIntMul = makePrim "int*" primitiveIntMul
primitiveIntMul :: [BardValue] -> BardValue
primitiveIntMul [(BVInteger (BInteger m)),(BVInteger (BInteger n))] = (int (m*n))

----------------------------------------------------------------------
-- float*
----------------------------------------------------------------------

primFloatMul = makePrim "float*" primitiveFloatMul
primitiveFloatMul :: [BardValue] -> BardValue
primitiveFloatMul [(BVFloat (BFloat m)),(BVFloat (BFloat n))] = (float (m*n))

----------------------------------------------------------------------
-- int-
----------------------------------------------------------------------

primIntSub = makePrim "int-" primitiveIntSub
primitiveIntSub :: [BardValue] -> BardValue
primitiveIntSub [(BVInteger (BInteger m)),(BVInteger (BInteger n))] = (int (m-n))

----------------------------------------------------------------------
-- float-
----------------------------------------------------------------------

primFloatSub = makePrim "float-" primitiveFloatSub
primitiveFloatSub :: [BardValue] -> BardValue
primitiveFloatSub [(BVFloat (BFloat m)),(BVFloat (BFloat n))] = (float (m-n))

----------------------------------------------------------------------
-- make-sequence
----------------------------------------------------------------------

primMakeSeq = makePrim "make-sequence" primitiveMakeSequence

primitiveMakeSequence :: [BardValue] -> BardValue

primitiveMakeSequence [] = (BVSequence (BSequence S.empty))
primitiveMakeSequence vals = (BVSequence (BSequence (S.fromList vals)))

----------------------------------------------------------------------
-- make-map
----------------------------------------------------------------------

primMakeMap = makePrim "make-map" primitiveMakeMap

primitiveMakeMap :: [BardValue] -> BardValue

primitiveMakeMap [] = (BVMap (BMap M.empty))
primitiveMakeMap vals = (BVMap (BMap (M.fromList (plistToAlist vals))))

plistToAlist [] = []
plistToAlist (a:b:rest) = (a,b):(plistToAlist rest)
