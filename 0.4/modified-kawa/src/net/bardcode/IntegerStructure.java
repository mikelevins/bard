// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// IntegerStructure is a singleton that represents Integer values

public class IntegerStructure extends NativeStructure {

    private static IntegerStructure instance=null;

    IntegerStructure(){
        super("integer",gnu.math.IntNum.class);
    }
    
    public Object apply1(gnu.math.IntNum arg){return arg;}
    
    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            if (arg instanceof gnu.math.IntNum) {
                return apply1((gnu.math.IntNum)arg);
            } else {
                throw new IllegalArgumentException("Non-integer argument in integer constructor");
            }
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to integer constructor");
        }
    }

    public static IntegerStructure getInstance(){
        if (instance == null) { instance=new IntegerStructure(); }
        return instance;
    }
}

