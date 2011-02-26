module Environment
    where

import Data.List as L
import Data.Map as M

import Value

-------------------------------------------------
-- Environments
-------------------------------------------------

data Environment = Env (M.Map String BardValue)

emptyEnvironment = Env (M.empty)

instance Show Environment where
    show (Env m) = "{" ++ (expand m) ++ "}"
        where expand m = concat (L.intersperse " " (Prelude.map showPair (M.assocs m)))
              showPair (k, v) = (show k) ++ " " ++ (show v)
