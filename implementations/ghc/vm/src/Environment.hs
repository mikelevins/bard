module Environment
  where

import qualified Data.Map as M

import Expressions

-------------------------------------------------
-- environment
-------------------------------------------------

data Environment = Env (M.Map Expression Expression) deriving (Show, Eq)

-------------------------------------------------
-- standard environment
-------------------------------------------------

nullEnvironment = Env M.empty
standardEnvironment = nullEnvironment
