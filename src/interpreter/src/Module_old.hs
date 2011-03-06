module Module
    where

import Control.Concurrent.STM
import Control.Monad.STM
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Sequence as S

import Value

type Variable = TVar BardValue
type VariableName = String
type ModuleName = String

makeVariable :: BardValue -> STM Variable
makeVariable val = newTVar val

getValue :: Variable -> STM BardValue
getValue var = readTVar var

setValue :: Variable -> BardValue -> STM ()
setValue var val = writeTVar var val

type VariableTable = M.Map VariableName (STM Variable)

makeVarTable :: [VariableName] -> VariableTable
makeVarTable varlist = M.fromList (L.zip varlist (L.map (\x -> (makeVariable (BardUndefined))) varlist)) 

getVar :: VariableName -> Module -> Maybe (STM Variable)
getVar vname (Module vtbl _) = M.lookup vname vtbl

putVar :: VariableName -> Module -> VariableTable
putVar vname mod@(Module vtable _) = 
    case (getVar vname mod) of
      Nothing -> (M.insert vname (makeVariable BardUndefined) vtable)
      Just v -> vtable

data Module = Module VariableTable ExportList
type ExportList = [VariableName]
type ImportList = [Import]
data Import = Import ModuleName VariableName Alias
type Alias = VariableName 

parseImportList :: [Import] -> ModuleTable -> VariableTable
parseImportList imports mtable = 
    M.fromList (extractVars (collectVars (fetchVars imports)))
    where fetchVars imps = (L.map (\(Import m v a) -> (a,(getVar v (findModule m mtable)))) 
                             imps)
          collectVars vspecs = (L.filter (\(a,v) -> case v of
                                                      Nothing -> False
                                                      Just x -> True)
                               vspecs)
          extractVars vspecs = (L.map (\(a, v) -> (a, (fromJust v)))
                               vspecs)

makeModule :: ExportList -> ImportList -> ModuleTable -> Module
makeModule exports imports mtable = let etable = makeVarTable exports
                                        itable = parseImportList imports mtable 
                                        vtable = M.union itable etable
                                    in Module vtable exports

type ModuleMap = M.Map ModuleName Module
type ModuleTable = TVar ModuleMap

createModuleTable :: STM ModuleMap
createModuleTable = newTVar (M.empty)

findModule :: ModuleName -> ModuleTable -> STM Module
findModule mname mtable = do
  mmap <- readTVar mtable
  return (fromJust (M.lookup mname mmap))

