module Module
    where

import Data.Sequence as S
import Data.Map as M

-------------------------------------------------
-- Modules
-------------------------------------------------

type ModuleName = String
type NameList = S.Seq String
type ExportsList = S.Seq String
type ImportsTable = M.Map ModuleName NameList

nameList :: [String] -> NameList
nameList nms = S.fromList nms

data Module = NoModule
            | Module NameList ExportsList ImportsTable
              deriving (Ord,Eq)

instance Show Module where
    show m = "#<Bard Module>"

makeModule :: [String] -> [String] -> [(ModuleName, NameList)] -> Module 
-- FIXME: this code makes it possible to create a module with
--        exports that don't exist in the modules namelist.
makeModule names exports imports = Module (nameList names) (nameList exports) (M.fromList imports)

data Modules = Modules (M.Map String Module)

instance Show Modules where
    show (Modules ms) = "#<Module Table>"

standardModules :: Modules
standardModules = Modules (M.fromList [("bard.core", (makeModule [] [] []))])

