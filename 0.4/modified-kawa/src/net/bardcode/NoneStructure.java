// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import net.bardcode.Structure;

public class NoneStructure extends Structure {

    static NoneStructure instance=null; 
    
    public NoneStructure(){
        name="none";
    }

    public static NoneStructure getInstance(){
        if (instance==null) { instance = new NoneStructure(); }
        return instance;
    }

    @Override
    public Object applyN(Object[] args){
        return Structure.nothing;
    }
}

