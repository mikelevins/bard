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

eval (BardName (vname,mname)) mmgr = do
	v <- getVar mmgr mname vname
	get v

