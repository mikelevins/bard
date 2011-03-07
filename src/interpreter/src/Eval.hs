module Eval
    where

import Control.Monad.STM
import Data.Map as M

import Box
import Env
import Module
import Name
import Value

eval :: BardValue -> Env -> ModuleManager -> STM BardValue 

-- self-evaluating
eval val@(BVUndefined _) _ _ = do return val
eval val@(BVNothing _) _ _ = do return  val
eval val@(BVBoolean _) _ _ = do return  val
eval val@(BVInteger _) _ _ = do return  val
eval val@(BVFloat _) _ _ = do return  val
eval val@(BVCharacter _) _ _ = do return  val
eval val@(BVText _) _ _ = do return  val
eval val@(BVBox _) _ _ = do return  val
eval val@(BVMap _) _ _ = do return  val

-- variable references
eval var@(BVName (BName mname vname)) env mmgr = do
	if (mname == "bard.keyword")
		then return var
		else if (mname == "")
			then evalUnqualifiedVarName vname env mmgr
			else evalQualifiedVarName vname mname env mmgr

evalQualifiedVarName :: VariableName -> ModuleName -> Env -> ModuleManager -> STM BardValue
evalQualifiedVarName vname mname env mmgr = do
  case (M.lookup (BName mname vname) env) of
    Nothing -> do var <- getVar mmgr mname vname
	          (get var)
    Just val -> return val

evalUnqualifiedVarName :: VariableName -> Env -> ModuleManager -> STM BardValue
evalUnqualifiedVarName vname env mmgr = do
  currm <- getCurrentModule mmgr
  let (BVText (BText mname)) = currm
  case (M.lookup (BName mname vname) env) of
    Nothing -> do var <- getVar mmgr mname vname
	          (get var)
    Just val -> return val