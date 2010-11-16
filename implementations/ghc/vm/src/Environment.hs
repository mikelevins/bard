module Environment
  where

import qualified Data.Map as M

import Expressions

-------------------------------------------------
-- environment
-------------------------------------------------

data Environment = Env (M.Map Expression Expression) deriving (Show, Eq)

-------------------------------------------------
-- constructors
-------------------------------------------------

extendEnv :: Environment -> Expression -> Expression -> Environment
extendEnv (Env env) nm val = Env (M.insert nm val env)

lookupName (Env env) nm = env M.! nm

-------------------------------------------------
-- accessors
-------------------------------------------------

-------------------------------------------------
-- standard environment
-------------------------------------------------

nullEnvironment = Env M.empty
standardEnvironment = (extendEnv nullEnvironment (ExpName "nothing") ExpNothing)

