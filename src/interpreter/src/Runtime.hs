module Runtime
    where

import Data.Map as M
import Data.Maybe

import Value

initBard :: BardRuntime
initBard = BardRuntime {moduleInfo = initMInfo, env = initEnv}

data BardRuntime = BardRuntime {moduleInfo :: ModuleInfo, env :: Env}

type ModuleInfo = M.Map ModuleName Module

type Module = M.Map String BardValue

type Env = M.Map VariableName BardValue

makeModule :: [(String,BardValue)] -> Module
makeModule inits = M.fromList inits

initMInfo :: ModuleInfo
initMInfo = M.fromList [("bard.core",(makeModule [("*module*",Value.undefined)]))]

initEnv :: Env
initEnv = M.empty

getCurrentModuleName :: BardRuntime -> String
getCurrentModuleName bard = 
     case (getMVar bard "bard.core" "*module*") of
       (BVText (BText txt)) -> txt
       _ -> "bard.core"
       
        

getMVar bard mname vname = fromJust (M.lookup vname (findModule bard mname))

findModule bard mname = fromJust (M.lookup mname (moduleInfo bard))