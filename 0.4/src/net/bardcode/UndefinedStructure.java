// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import net.bardcode.Structure;

public class UndefinedStructure extends Structure {

    static UndefinedStructure instance=null; 
    
    public UndefinedStructure(){
        name="undefined";
    }

    public static UndefinedStructure getInstance(){
        if (instance==null) { instance = new UndefinedStructure(); }
        return instance;
    }

    @Override
    public Object applyN(Object[] args){
        return Structure.undefined;
    }
}

