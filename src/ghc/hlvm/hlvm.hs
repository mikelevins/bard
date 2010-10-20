module Main
  where

import qualified Data.Map as M
import qualified Data.Sequence as S

-------------------------------------------------
-- bard values
-------------------------------------------------

type Key = Value

type Name =  String

type Var = Name

type Category = Name

type Signature = [Category]

type ReturnType = [Category] 

data Function = Function Name Signature ReturnType deriving Show

type Method = (Signature, Value)

type Protocol = (String, [Function])

type Module = (M.Map Var Value)

data Value = NothingVal
           | CellVal Value
           | TrueVal
           | FalseVal
           | IntegerVal Integer
           | CharVal Char
           | NameVal Name
           | StructureVal [(Name, Value)]
           | SequenceVal (S.Seq Value)
           | MapVal (M.Map Key Value)
           | CategoryVal Category
           | FunctionVal Function
           | MethodVal Method
           | ProtocolVal Protocol 
           | ModuleVal Module deriving Show

-------------------------------------------------
-- the virtual machine
-------------------------------------------------

type VM = (Run, Exp, Env, Values)

type Run = Bool
type Exp = Value
data Env = Env (M.Map Var Value) deriving Show
nullEnv = Env M.empty

type Values = [Value]

-- VM execution 

getop :: Exp -> Env -> Value
getop exp env = NothingVal -- TODO: implement this properly

apply :: Value -> VM -> VM
apply fn vm = vm -- TODO: implement this properly

hlvm :: VM -> VM
hlvm (run, exp, env, outvals) =
    if run
    then let vm = (run, exp, env, outvals)
         in hlvm (apply (getop exp env) vm)
    else (run, exp, env, outvals)

-------------------------------------------------
-- main program
-------------------------------------------------

main = do
       putStrLn "Bard VM v 1.0"
       putStrLn (show result)
       where
         result  = hlvm (True, NothingVal, nullEnv, [])
