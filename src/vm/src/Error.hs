module Error
    where

import Expression

-------------------------------------------------
-- Errors
-------------------------------------------------

data Error = NoError
           | InvalidExpression Expression 
           | UndefinedVariable Expression 
           | UndefinedOperator Expression 
             deriving Show