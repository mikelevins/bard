module Eval
    where

import Value

eval :: BardValue -> BardValue 
eval BardUndefined = BardUndefined
eval BardNothing = BardNothing
eval val@(BardBoolean _) = val
eval val@(BardInteger _) = val
eval val@(BardFloat _) = val
eval val@(BardCharacter _) = val
eval val@(BardText _) = val

