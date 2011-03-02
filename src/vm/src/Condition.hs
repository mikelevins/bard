module Condition
    where

import Expression

-------------------------------------------------
-- Conditions
-------------------------------------------------

data Condition = NoError
               | InvalidExpression Expression 
               | UndefinedVariable Expression 
               | UndefinedOperator Expression 
                 deriving Show