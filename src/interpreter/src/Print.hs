module Print
    where

import Data.List as L
import Data.Map as M
import Data.Sequence as S

import Name
import Value

instance Show BardValue where
    show (BVUndefined _) = "undefined"
    show (BVNothing _) = "nothing"
    show (BVBoolean BTrue) = "true"
    show (BVBoolean BFalse) = "false"
    show (BVInteger (BInteger n)) = (show n)
    show (BVFloat (BFloat f)) = (show f)
    show (BVCharacter (BCharacter ch)) = ['\\',ch]
    show (BVText (BText tx)) = ['"']++tx++['"']
    show (BVName (BName "bard.keyword" nm)) = (":"++nm)
    show (BVName (BName "" nm)) = nm
    show (BVName (BName mnm nm)) = (mnm++":"++nm)
    show (BVMap (BMap m)) = "{" ++ (expand m) ++ "}"
        where expand m = L.concat (L.intersperse " " (Prelude.map showPair (M.assocs m)))
              showPair (k, v) = (show k) ++ " " ++ (show v)
    show (BVSequence (BSequence s)) = "(" ++ (expand s) ++ ")"
        where expand s = L.concat (L.intersperse " " (Prelude.map showNth (Prelude.take (S.length s) [0 ..])))
              showNth i = (show (index s i))
    show (BVPrim (BPrim pname _)) = "#<bard primitive \""++pname++"\">"
