module Env 
    where

import Data.Map as M

import Value

type Env = M.Map BName BardValue

emptyEnv :: Env
emptyEnv = M.empty

extendEnv :: Env -> Env -> Env
extendEnv env ext = M.union ext env

makeEnv :: [(BName,BardValue)] -> Env
makeEnv initList = M.fromList initList

standardEnv :: Env
standardEnv = emptyEnv