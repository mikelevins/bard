module Module
    where

type Variable = MVar BardValue
type Module = M.Map Name Variable
type ModuleTable = M.Map ModuleName Module
type ModuleName = String
