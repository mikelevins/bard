module Module
    where

import Control.Concurrent.STM
import Control.Monad.STM
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Sequence as S

import Box
import Value
import Prims
import Print

type MVarList = M.Map MVarID BardValue
type MVarID = Integer

data BName = MInternedName String
           | MImportedName String Module String

data Module = Module {
      moduleName :: String,
      moduleName 
    }

