// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import net.bardcode.BardClass;

// ClassStructure is a singleton that represents the structure of classes

public class ClassStructure extends NativeStructure {

    private static ClassStructure instance=null;

    ClassStructure(){
        super("class",BardClass.class);
    }
    
    public BardClass findOrMakeClass(gnu.mapping.Symbol className){
        BardClass foundClass = BardClass.findClass(className);
        if ( foundClass == null ){ foundClass = new BardClass(className); }
        return foundClass;
    }

    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            gnu.mapping.Symbol sym = (gnu.mapping.Symbol)arg;
            return findOrMakeClass(sym); 
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to box constructor");
        }
    }


    public static ClassStructure getInstance(){
        if (instance == null) { instance=new ClassStructure(); }
        return instance;
    }
}

