module TestPrograms
    where

import Data.Map as M
import Data.Sequence as S

import Environment
import Condition
import Module
import Value
import VM

-------------------------------------------------
-- test value printers
-------------------------------------------------

testValuePrinters = do
       putStrLn "Test value printers:"
       putStrLn ""
       putStrLn ("  Undefined: " ++ (show BardUndefined))
       putStrLn ("  Nothing: " ++ (show nothing))
       putStrLn ("  Booleans: ")
       putStrLn ("     " ++ (show (bool True)))
       putStrLn ("     " ++ (show (bool False)))
       putStrLn ("  Integers: ")
       putStrLn ("     " ++ (show (int 1)))
       putStrLn ("     " ++ (show (int 123456789)))
       putStrLn ("  Floats: ")
       putStrLn ("     " ++ (show (float 1.0)))
       putStrLn ("     " ++ (show (float 0.123456789)))
       putStrLn ("  Characters: ")
       putStrLn ("     " ++ (show (char 'C')))
       putStrLn ("     " ++ (show (char '!')))
       putStrLn ("  Names: ")
       putStrLn ("     " ++ (show (name "" "foo")))
       putStrLn ("     " ++ (show (name "" "*bard-version*")))
       putStrLn ("  Sequences: ")
       putStrLn ("     " ++ (show (Value.sequence [])))
       putStrLn ("     " ++ (show (Value.sequence [(int 0),(int 1),(int 2)])))
       putStrLn ("  Maps: ")
       putStrLn ("     " ++ (show (Value.map [])))
       putStrLn ("     " ++ (show (Value.map [((name "" "zero"), (BardInteger 0)),
                                              ((name "" "one"), (BardInteger 1)),
                                              ((name "" "two"), (BardInteger 2))])))
       putStrLn ""


-------------------------------------------------
-- test evaluators
-------------------------------------------------

testEvaluators = do
       putStrLn "Test evaluators:"
       putStrLn ""
       putStrLn "  undefined:"
       putStrLn ("   in:  " ++ (show ((BardUndefined),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((BardUndefined),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn "  nothing:"
       putStrLn ("   in:  " ++ (show ((nothing),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((nothing),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn "  Booleans:"
       putStrLn ("   in:  " ++ (show ((bool True),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((bool True),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((bool False),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((bool False),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn "  Integers:"
       putStrLn ("   in:  " ++ (show ((int 0),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((int 0),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((int 123456789),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((int 123456789),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn "  Floats:"
       putStrLn ("   in:  " ++ (show ((float 1.2),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((float 1.2),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((float 1.23456789),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((float 1.23456789),(standardEnvironment),
                                            (standardModules),(NoError)))))
       putStrLn ""
       putStrLn "  Characters:"
       putStrLn ("   in:  " ++ (show ((char 'C'),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((char 'C'),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((char '\t'),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((char '\t'),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn "  Names:"
       putStrLn ("   in:  " ++ (show ((name "" "Foo"),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((name "" "Foo"),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((name "bard.core" "*bard-version*"),(standardEnvironment),
                                      (standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((name "bard.core" "*bard-version*"),(standardEnvironment),
                                            (standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((name "bard.core" "Foo"),(standardEnvironment),
                                      (standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((name "bard.core" "Foo"),(standardEnvironment),
                                            (standardModules),(NoError)))))
       putStrLn ""
       putStrLn "  Sequences:"
       putStrLn ("   in:  " ++ (show ((emptySequence),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((emptySequence),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((Value.sequence [(name "" "+"),(int 1),(int 2)]),
                                      (standardEnvironment),(standardModules),
                                      (NoError))))
       putStrLn ("   out: " ++ (show (eval ((Value.sequence [(name "" "+"),(int 1),(int 2)]),
                                            (standardEnvironment),(standardModules),
                                            (NoError)))))
       putStrLn ""
       putStrLn "  Maps:"
       putStrLn ("   in:  " ++ (show ((emptyMap),(standardEnvironment),(standardModules),(NoError))))
       putStrLn ("   out: " ++ (show (eval ((emptyMap),(standardEnvironment),(standardModules),(NoError)))))
       putStrLn ""
       putStrLn ("   in:  " ++ (show ((Value.map [((name "" "zero"),(int 0)),
                                                  ((name "" "one"),(int 1)),
                                                  ((name "" "two"),(int 2))]),
                                      (standardEnvironment),(standardModules),
                                      (NoError))))
       putStrLn ("   out: " ++ (show (eval ((Value.map [((name "" "zero"),(int 0)),
                                                        ((name "" "one"),(int 1)),
                                                        ((name "" "two"),(int 2))]),
                                      (standardEnvironment),(standardModules),
                                      (NoError)))))
       putStrLn ""
                                                                                                                                                                                                                                