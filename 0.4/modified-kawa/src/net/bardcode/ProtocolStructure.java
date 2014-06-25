// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import net.bardcode.Protocol;
import net.bardcode.Function;

// ProtocolStructure is a singleton that represents the structure of protocols

public class ProtocolStructure extends NativeStructure {

    private static ProtocolStructure instance=null;

    ProtocolStructure(){
        super("protocol",net.bardcode.Protocol.class);
    }
    
    java.lang.Boolean isEven(int i){
        return ((i % 2) == 0 );
    }

    @Override
    public Object applyN(Object[] args){
        Protocol result = null;
        int count = args.length;
        if (count < 1) {
            throw new IllegalArgumentException("No arguments to protocol constructor");
        } else {
            Object firstArg = args[0];
            if (firstArg instanceof Symbol) {
                Symbol pname = (Symbol)firstArg;
                ArrayList fnArgs = new ArrayList();
                // collect the name/function arguments
                // there have to be an even number of them
                if (isEven(count-1)) {
                    for (int i=1; i<count; i++){
                        Object next = args[i];
                        // check that even-indexed args are symbols, odd-indexed args are functions
                        if (isEven(i)) {
                            if (!(next instanceof Function)) {
                                throw new IllegalArgumentException("Value arguments in the protocol constructor must be functions");
                            }
                        } else {
                            if (!(next instanceof Symbol)) {
                                throw new IllegalArgumentException("Key arguments in the protocol constructor must be symbols");
                            }
                        }
                        fnArgs.add(next);
                    }
                } else {
                    throw new IllegalArgumentException("Odd number of name/function arguments in the protocol constructor");
                }

                // create the protocol, then add the names and functions to its function map
                result = new Protocol(pname);
                int itemCount = fnArgs.size();
                for (int i=0; i<itemCount; i+=2){
                    Symbol key = (Symbol)fnArgs.get(i);
                    Function fn = (Function)fnArgs.get(i+1);
                    result.addFunction(key,fn);
                }
                
            } else {
                throw new IllegalArgumentException("The first argument to the protocol constructor must be a symbol");
            }
        }
        
        return result;
    }


    public static ProtocolStructure getInstance(){
        if (instance == null) { instance=new ProtocolStructure(); }
        return instance;
    }
}

