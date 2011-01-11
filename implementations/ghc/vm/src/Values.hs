module Values
    where

import Data.Sequence as S
import Data.Map as M

-------------------------------------------------
-- bard values
-------------------------------------------------

-- supporting types

data Env = Env (M.Map Name Value) deriving (Show, Eq)
data Module = Module Env deriving (Show, Eq)

type Character = Char

type Text = S.Seq Char

type Name = String

type Sequence = S.Seq Value

type Map = M.Map Value Value

data Value = ValNothing
           | ValBoolean Bool
           | ValInteger Integer
           | ValFloat Float
           | ValCharacter Character
           | ValText Text
           | ValName Name
           | ValModule Module
           | ValSequence Sequence
           | ValMap Values.Map
             deriving (Show, Eq)

