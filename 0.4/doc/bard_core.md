# Bard.Core

Bard.Core is an extended subset of Bard that serves as an intermediate language: a target for the Bard front-end compiler, and a source for its various back-ends. 

    (def name expr) => name

    (^ (param1 param2 ...) expr1 expr2 ...) => method

    ((setter name) expr) => val
    
    ((setter (name val)) expr) => val'
    
    (fn arg1 arg2 ...) => val
    
    (if test then else) => val
    
    var => val
    
    const => const
    
    
    
    