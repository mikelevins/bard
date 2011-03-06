module Module
    where

import Control.Concurrent.STM
import Control.Monad.STM
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Sequence as S

import Value

data ModuleManager = ModMgr (TVar ModuleMap) (TVar ModuleName)
type ModuleMap = M.Map ModuleName Module
type ModuleName = String
type Module = M.Map VariableName (STM Box)
type VariableName = String
type Box = TVar BardValue

newModuleMap :: ModuleMap
newModuleMap = M.empty

initModules :: STM ModuleManager
initModules = do
  mtable <- newTVar newModuleMap
  let mname = "bard.core"
  m <- newTVar mname
  let mmgr = ModMgr mtable m 
  addModule mmgr mname (makeModule [("*version*", (BardText "1.0"))])
  setCurrentModule mmgr mname
  return mmgr

getModuleMap :: ModuleManager -> STM ModuleMap
getModuleMap (ModMgr mtable _) = readTVar mtable

getCurrentModule :: ModuleManager -> STM ModuleName
getCurrentModule (ModMgr _ mname) = readTVar mname

setModuleMap :: ModuleManager -> ModuleMap -> STM ()
setModuleMap (ModMgr mtable _) mmap = writeTVar mtable mmap

setCurrentModule :: ModuleManager -> ModuleName -> STM ()
setCurrentModule (ModMgr _ mname) name = writeTVar mname name

addModule :: ModuleManager -> ModuleName -> Module -> STM ()
addModule mmgr mname m = do
  mmap <- getModuleMap mmgr
  setModuleMap mmgr (M.insert mname m mmap)

findModule :: ModuleManager -> ModuleName -> STM Module
findModule mmgr mname = do
  mmap <- getModuleMap mmgr
  return (fromJust (M.lookup mname mmap))

makeModule :: [(VariableName, BardValue)] -> Module
makeModule vnames = M.fromList (L.map (\(nm,val) -> (nm,(makeBox val))) vnames)

makeBox :: BardValue -> STM Box
makeBox val = newTVar val

get :: Box -> STM BardValue
get box = readTVar box

set :: Box -> BardValue -> STM ()
set box val = writeTVar box val