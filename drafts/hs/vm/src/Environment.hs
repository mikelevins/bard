module Environment
    where

import Data.List as L
import Data.Map as M

import Value

-------------------------------------------------
-- Environments
-------------------------------------------------

data Environment = Env (M.Map Name BardValue)

emptyEnvironment = Env (M.empty)
standardEnvironment = extendEnvironment emptyEnvironment (Name "bard.core" "*bard-version*") (BardFloat 1.0)

extendEnvironment :: Environment -> Name -> BardValue -> Environment
extendEnvironment (Env env) k v = (Env (M.insert k v env))

instance Show Environment where
    show (Env m) = "#<Bard Environment>"

