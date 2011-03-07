module Prims
    where

import Value

makePrim :: String -> ([BardValue] -> BardValue) -> BardValue
makePrim pname f = BVPrim (BPrim pname f)

primAdd = makePrim "+" primitiveAdd

----------------------------------------------------------------------
-- +
----------------------------------------------------------------------

primitiveAdd :: [BardValue] -> BardValue

primitiveAdd [] = (int 0)

primitiveAdd [(BVInteger (BInteger n))] = (int n)
primitiveAdd [(BVInteger (BInteger m)),(BVInteger (BInteger n))] = (int (m+n))

primitiveAdd [(BVFloat (BFloat n))] = (float n)
primitiveAdd [(BVFloat (BFloat m)),(BVFloat (BFloat n))] = (float (m+n))

primitiveAdd [(BVInteger (BInteger m)),(BVFloat (BFloat n))] = 
    let fm = fromIntegral m
    in (float (fm+n))

primitiveAdd [(BVFloat (BFloat m)),(BVInteger (BInteger n))] = 
    let fn = fromIntegral n
    in (float (m+fn))

----------------------------------------------------------------------
-- *
----------------------------------------------------------------------

primMul = makePrim "*" primitiveMul

primitiveMul :: [BardValue] -> BardValue

primitiveMul [] = (int 1)

primitiveMul [(BVInteger (BInteger n))] = (int n)
primitiveMul [(BVInteger (BInteger m)),(BVInteger (BInteger n))] = (int (m*n))

primitiveMul [(BVFloat (BFloat n))] = (float n)
primitiveMul [(BVFloat (BFloat m)),(BVFloat (BFloat n))] = (float (m*n))

primitiveMul [(BVInteger (BInteger m)),(BVFloat (BFloat n))] = 
    let fm = fromIntegral m
    in (float (fm*n))

primitiveMul [(BVFloat (BFloat m)),(BVInteger (BInteger n))] = 
    let fn = fromIntegral n
    in (float (m*fn))

----------------------------------------------------------------------
-- -
----------------------------------------------------------------------

primSub = makePrim "-" primitiveSub

primitiveSub :: [BardValue] -> BardValue

primitiveSub [] = (int 0)

primitiveSub [(BVInteger (BInteger n))] = (int (0 - n))
primitiveSub [(BVInteger (BInteger m)),(BVInteger (BInteger n))] = (int (m-n))

primitiveSub [(BVFloat (BFloat n))] = (float (0.0 - n))
primitiveSub [(BVFloat (BFloat m)),(BVFloat (BFloat n))] = (float (m-n))

primitiveSub [(BVInteger (BInteger m)),(BVFloat (BFloat n))] = 
    let fm = fromIntegral m
    in (float (fm-n))

primitiveSub [(BVFloat (BFloat m)),(BVInteger (BInteger n))] = 
    let fn = fromIntegral n
    in (float (m-fn))

