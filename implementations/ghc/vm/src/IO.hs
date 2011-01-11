module IO
    where

import Values
import Instructions

-------------------------------------------------
-- serialization and I/O
-------------------------------------------------

readProgram programFile =  [NOOP,
                            NOTHING,
                            TRUE,
                            FALSE,
                            MINUS_ONE,
                            ONE,
                            TWO,
                            HALT]
