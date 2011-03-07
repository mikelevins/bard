module Module
    where

import Control.Concurrent.STM
import Control.Monad.STM
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Sequence as S

import Box
import Value
import Name
import Prims

data ModuleManager = ModMgr (TVar ModuleMap)
type ModuleMap = M.Map ModuleName Module
type Module = M.Map VariableName (STM Box)
newModuleMap :: ModuleMap
newModuleMap = M.empty

initModules :: STM ModuleManager
initModules = do
  mtable <- newTVar newModuleMap
  let mmgr = ModMgr mtable
  let coremodule = makeModule [("*module*", (text "bard.core")),
                               ("*version*", (BVText (BText "1.0"))),
                               ("+", primAdd),
                               ("*", primMul),
                               ("-", primSub),
                               ("sequence", primSeq),
                               ("sequence->map", primSeqToMap)
                              ]
  addModule mmgr "bard.core" coremodule
  setCurrentModule mmgr (text "bard.core")
  return mmgr

getModuleMap :: ModuleManager -> STM ModuleMap
getModuleMap (ModMgr mtable) = readTVar mtable

setModuleMap :: ModuleManager -> ModuleMap -> STM ()
setModuleMap (ModMgr mtable) mmap = writeTVar mtable mmap

getCurrentModule :: ModuleManager -> STM BardValue
getCurrentModule mmgr = do
    v <- getVar mmgr "bard.core" "*module*"
    get v

setCurrentModule :: ModuleManager -> BardValue -> STM ()
setCurrentModule mmgr mname = do
    v <- getVar mmgr "bard.core" "*module*"
    set v mname

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

ensureVar :: ModuleManager -> ModuleName -> VariableName -> STM ()
ensureVar mmgr mname vname = do
  mmap <- getModuleMap mmgr
  let m = fromJust (M.lookup mname mmap)
  case (M.lookup vname m) of
    Nothing -> setModuleMap mmgr 
                            (M.insert mname 
                                      (M.insert vname 
                                                (makeBox (BVUndefined BUndefined)) 
                                                m)
                                      mmap)
    Just v -> return ()

getVar :: ModuleManager -> ModuleName -> VariableName -> STM Box
getVar mmgr mname vname = do
  mmap <- getModuleMap mmgr
  let m = fromJust (M.lookup mname mmap)
  fromJust (M.lookup vname m)

setVar :: ModuleManager -> ModuleName -> VariableName -> BardValue -> STM ()
setVar mmgr mname vname val = do
  ensureVar mmgr mname vname
  v <- getVar mmgr mname vname
  set v val
  
intern :: VariableName -> ModuleName -> ModuleManager -> STM BardValue
intern vname mname mmgr = do
  ensureVar mmgr mname vname
  return (BVName (BName mname vname))

