module Values
    where

import qualified Data.Map as M

-------------------------------------------------
-- bard values
-------------------------------------------------

type Name = String

type Arglist = [Name]

type Names = [Name]
type ModuleName = Name
type ExportedNames = [Name]
type ImportedNames = (M.Map ModuleName Names)

data Value = NothingVal
           | IntegerVal Int
           | FloatVal Float
           | CharVal Char
           | NameVal String
           | SeqVal [Value]
           | MapVal (M.Map Value Value)
           | ModuleVal Names ExportedNames ImportedNames deriving Show

