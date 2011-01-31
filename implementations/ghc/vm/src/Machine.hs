module Machine
    where

import Data.Map as M
import Data.Sequence as S

import Values

-------------------------------------------------
-- the Environment
-------------------------------------------------

lookupVariableValue :: Value -> Env -> Value
-- TODO: implement variable lookup
lookupVariableValue exp env = ValNothing

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Value, Env)

-- apply

apply :: Value -> Value -> Env -> (Value, Env)
-- TODO: implement apply
apply op args env = (args,env)

-- eval

evalConstant :: VM -> VM
evalConstant (exp,env) = (exp,env)

evalVariable :: VM -> VM
evalVariable (exp,env) = ((lookupVariableValue exp env), env)

evalSequence :: VM -> VM
evalSequence (exp,env) = 
    let opform = S.index exp 0
        args = S.drop 1 exp
        (op, env') = eval opform env
    in apply op args env'

evaluator :: Value -> (VM -> VM)
evaluator val = 
    case val of
      ValNothing -> evalConstant
      ValBoolean b -> evalConstant
      ValInteger n -> evalConstant
      ValFloat f -> evalConstant
      ValCharacter c -> evalConstant
      ValText t -> evalConstant
      ValName n -> evalVariable
      ValModule m -> evalConstant
      ValSequence s ->evalSequence
      ValMap m -> evalConstant

-- execution: run

eval :: VM -> VM
eval (exp,env) =
    let eval' = evaluator(exp)
    in eval'(exp, env)

-- initialization

makeStandardEnvironment = Env (M.empty)

makeVM :: Value -> VM
makeVM program = (program,(makeStandardEnvironment))
