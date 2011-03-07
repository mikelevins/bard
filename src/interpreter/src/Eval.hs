module Eval
    where

import Control.Monad
import Control.Monad.STM
import Data.Foldable as F
import Data.List as L
import Data.Map as M
import Data.Sequence as S
import Data.Traversable as T

import Box
import Env
import Module
import Name
import Value

---------------------------------------------------------------------
-- eval
---------------------------------------------------------------------

eval :: BardValue -> Env -> ModuleManager -> STM BardValue 

-- variable references
eval var@(BVName (BName mname vname)) env mmgr = do
	if (mname == "bard.keyword")
		then return var
		else if (mname == "")
			then evalUnqualifiedVarName vname env mmgr
			else evalQualifiedVarName vname mname env mmgr

-- sequences: applications
eval (BVSequence (BSequence s)) env mmgr = do 
  let op = (S.index s 0)
  let args = (S.drop 1 s)
  f <- eval op env mmgr
  parms <- T.mapM (\a -> eval a env mmgr) args
  (apply f parms)

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

---------------------------------------------------------------------
-- apply
---------------------------------------------------------------------

apply :: BardValue -> (S.Seq BardValue) -> STM BardValue
apply (BVPrim (BPrim pname pop)) args = do
  let params = F.toList args
  return (pop params)

---------------------------------------------------------------------
-- support functions
---------------------------------------------------------------------

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

