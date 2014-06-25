package net.bardcode;
import gnu.mapping.*;
import org.pcollections.HashPMap;
import org.pcollections.HashTreePMap;
import net.bardcode.Function;

public class Protocol {
    Symbol name;
    HashPMap<Symbol,Function> functions;

    public Protocol(Symbol nm){
        name = nm;
        functions = HashTreePMap.empty();
    }

    public Symbol getName(){
        return name;
    }

    public HashPMap<Symbol,Function> getFunctions(){
        return functions;
    }

    public Protocol addFunction(Symbol nm , Function fn){
        functions = functions.plus(nm,fn);
        return this;
    }
}


