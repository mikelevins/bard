module IO
    where

import Data.Sequence as S

import Values

-------------------------------------------------
-- serialization and I/O
-------------------------------------------------

readProgram programFile =  ValSequence (S.fromList [])
