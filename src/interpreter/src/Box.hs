module Box
    where

import Control.Concurrent.STM
import Control.Monad.STM

import Value

makeBox :: BardValue -> STM Box
makeBox val = newTVar val

get :: Box -> STM BardValue
get box = readTVar box

set :: Box -> BardValue -> STM ()
set box val = writeTVar box val

