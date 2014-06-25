// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// BooleanStructure is a singleton that represent Boolean values

public class BooleanStructure extends NativeStructure {

    private static BooleanStructure instance=null;

    BooleanStructure(){
        super("boolean",java.lang.Boolean.class);
    }
    
    public Object apply1(java.lang.Boolean arg){return arg;}
    
    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            if (arg instanceof java.lang.Boolean) {
                return apply1((java.lang.Boolean)arg);
            } else {
                throw new IllegalArgumentException("Non-Boolean argument in boolean constructor");
            }
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to boolean constructor");
        }
    }

    public static BooleanStructure getInstance(){
        if (instance == null) { instance=new BooleanStructure(); }
        return instance;
    }
}

