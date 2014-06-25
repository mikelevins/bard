// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import gnu.lists.*;
import java.util.*;

// VectorStructure is a singleton that represents vector values

public class VectorStructure extends NativeStructure {

    private static VectorStructure instance=null;

    VectorStructure(){
        super("vector", Vector.class);
    }

    @Override
    public Object applyN(Object[] args){
        return new FVector(Arrays.asList(args));
    }


    public static VectorStructure getInstance(){
        if (instance == null) { instance=new VectorStructure(); }
        return instance;
    }
}

