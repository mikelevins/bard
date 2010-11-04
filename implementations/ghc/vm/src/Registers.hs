module Registers
    where

import qualified Data.Map as M

import Instructions
import Values

-------------------------------------------------
-- VM register types
-------------------------------------------------

type Run = Bool
type Code = [Instruction]
data Environment = Env (M.Map Name Value) deriving Show


