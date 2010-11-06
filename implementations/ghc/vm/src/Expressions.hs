module Expressions
    where

import qualified Data.Map as M

-------------------------------------------------
-- bard expressions
-------------------------------------------------

data Expression = ExpNothing
                | ExpInt Int
                | ExpFloat Float
                | ExpChar Char
                | ExpName String
                | ExpSeq [Expression]
                | ExpMap (M.Map Expression Expression) deriving (Show, Read, Ord, Eq)


