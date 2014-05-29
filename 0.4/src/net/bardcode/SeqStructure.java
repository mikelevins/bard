// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import java.util.Arrays;

// SeqStructure is a singleton that represents the structure of seqs

public class SeqStructure extends NativeStructure {

    private static SeqStructure instance=null;

    SeqStructure(){
        super("seq",org.pcollections.TreePVector.class);
    }
    
    @Override
    public Object applyN(Object[] args){
        return org.pcollections.TreePVector.from(Arrays.asList(args));
    }


    public static SeqStructure getInstance(){
        if (instance == null) { instance=new SeqStructure(); }
        return instance;
    }
}

