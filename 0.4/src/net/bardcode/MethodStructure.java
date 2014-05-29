// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// MethodStructure is a singleton that represents method values

public class MethodStructure extends NativeStructure {

    private static MethodStructure instance=null;

    MethodStructure(){
        super("method", MethodProc.class);
    }

    @Override
    public Object applyN(Object[] args){
        // we never actually call this because method is a syntax form
        return null;
    }


    public static MethodStructure getInstance(){
        if (instance == null) { instance=new MethodStructure(); }
        return instance;
    }
}

