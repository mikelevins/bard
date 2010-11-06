module Environment
  where

import qualified Data.Map as M

import Expressions

-------------------------------------------------
-- environment
-------------------------------------------------

data Environment = Env (M.Map Expression Expression) deriving Eq

-------------------------------------------------
-- standard environment
-------------------------------------------------

nullEnvironment = Env M.empty
standardEnvironment = nullEnvironment
