module Print
    where

import Data.List as L
import Data.Map as M
import Data.Sequence as S

import Name
import Value

instance Show BardValue where
    show BardUndefined = "undefined"
    show BardNothing = "nothing"
    show (BardBoolean True) = "true"
    show (BardBoolean False) = "false"
    show (BardInteger n) = (show n)
    show (BardFloat f) = (show f)
    show (BardCharacter ch) = ['\\',ch]
    show (BardText tx) = ['"']++tx++['"']
    show (BardName (nm,"bard.keyword")) = (":"++nm)
    show (BardName (nm,"")) = nm
    show (BardName (nm,mnm)) = (mnm++":"++nm)
    show (BardMap m) = "{" ++ (expand m) ++ "}"
        where expand m = L.concat (L.intersperse " " (Prelude.map showPair (M.assocs m)))
              showPair (k, v) = (show k) ++ " " ++ (show v)
    show (BardSequence s) = "(" ++ (expand s) ++ ")"
        where expand s = L.concat (L.intersperse " " (Prelude.map showNth (Prelude.take (S.length s) [0 ..])))
              showNth i = (show (index s i))
