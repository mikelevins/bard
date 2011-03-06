module Eval
    where

import Control.Monad.STM

import Box
import Module
import Value

eval :: BardValue -> ModuleManager -> STM BardValue 

eval BardUndefined _ = do return BardUndefined
eval BardNothing _ = do return  BardNothing
eval val@(BardBoolean _) _ = do return  val
eval val@(BardInteger _) _ = do return  val
eval val@(BardFloat _) _ = do return  val
eval val@(BardCharacter _) _ = do return  val
eval val@(BardText _) _ = do return  val

eval var@(BardName (vname,mname)) mmgr = do
	if (mname == "bard.keyword")
		then return var
		else if (mname == "")
			then evalUnqualifiedVarName vname mmgr
			else evalQualifiedVarName vname mname mmgr

evalUnqualifiedVarName vname mmgr = do
	currm <- getCurrentModule mmgr
	var <- getVar mmgr currm vname
	get var
	
evalQualifiedVarName vname mname mmgr = do
	var <- getVar mmgr mname vname
	get var