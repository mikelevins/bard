module VM
    where

import Data.List as L
import Data.Map as M
import Data.Sequence as S

import Environment
import Error
import Expression
import Value

-------------------------------------------------
-- VM
-------------------------------------------------

type VM = (Expression,Environment,Error)

evaluateUnrecognized :: VM -> VM
evaluateUnrecognized (exp, env, _) = ((BardUndefined), env, (InvalidExpression exp))

evaluateSelfEvaluating :: VM -> VM
evaluateSelfEvaluating (exp, env, err) = (exp, env, err)

evaluateVariableReference :: VM -> VM
evaluateVariableReference (exp, env, err) = ((BardUndefined), env, (UndefinedVariable exp))

evaluateSequence :: VM -> VM
evaluateSequence (exp, env, err) = ((BardUndefined), env, (UndefinedOperator exp))

getEvaluator :: Expression -> Environment -> (VM -> VM)
-- self-evaluating expressions
getEvaluator (BardUndefined) _ = evaluateSelfEvaluating
getEvaluator (BardNothing) _ = evaluateSelfEvaluating
getEvaluator (BardBoolean _) _ = evaluateSelfEvaluating
getEvaluator (BardInteger _) _ = evaluateSelfEvaluating
getEvaluator (BardFloat _) _ = evaluateSelfEvaluating
getEvaluator (BardCharacter _) _ = evaluateSelfEvaluating
getEvaluator (BardMap _) _ = evaluateSelfEvaluating
-- expressions to evaluate
getEvaluator (BardName _) _ = evaluateVariableReference
getEvaluator (BardSequence _) _ = evaluateSequence

eval :: VM -> VM
eval (exp, env, err) = (getEvaluator exp env)(exp,env,err)