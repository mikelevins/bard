module VM
    where

import Data.List as L
import Data.Map as M
import Data.Sequence as S

import Condition
import Environment
import Expression
import Module
import Value

-------------------------------------------------
-- VM
-------------------------------------------------

type VM = (Expression,Environment,Modules,Condition)

evaluateUnrecognized :: VM -> VM
evaluateUnrecognized (exp, env, mods, _) = ((BardUndefined), env, mods, (InvalidExpression exp))

evaluateSelfEvaluating :: VM -> VM
evaluateSelfEvaluating (exp, env, mods, err) = (exp, env, mods, err)

evaluateVariableReference :: VM -> VM
evaluateVariableReference ((BardName (Name "" nm)), (Env env), mods, err) = 
    ((BardName (Name "" nm)), (Env env), mods, NoError)
evaluateVariableReference ((BardName (Name m nm)), (Env env), mods, err) = 
    case (M.lookup (Name m nm) env) of
      Nothing -> ((BardUndefined), (Env env), mods, (UndefinedVariable (BardName (Name m nm))))
      Just val -> (val, (Env env), mods, NoError)

evaluateSequence :: VM -> VM
evaluateSequence (exp, env, mods, err) = ((BardUndefined), env, mods, (UndefinedOperator exp))

getEvaluator :: Expression -> (VM -> VM)
-- self-evaluating expressions
getEvaluator (BardUndefined) = evaluateSelfEvaluating
getEvaluator (BardNothing) = evaluateSelfEvaluating
getEvaluator (BardBoolean _) = evaluateSelfEvaluating
getEvaluator (BardInteger _) = evaluateSelfEvaluating
getEvaluator (BardFloat _) = evaluateSelfEvaluating
getEvaluator (BardCharacter _) = evaluateSelfEvaluating
getEvaluator (BardMap _) = evaluateSelfEvaluating
-- expressions to evaluate
getEvaluator (BardName _) = evaluateVariableReference
getEvaluator (BardSequence _) = evaluateSequence

eval :: VM -> VM
eval (exp, env, mods, err) = (getEvaluator exp)(exp,env, mods,err)