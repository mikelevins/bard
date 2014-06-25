// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// FloatStructure is a singleton that represents floating-point values

public class FloatStructure extends NativeStructure {

    private static FloatStructure instance=null;

    FloatStructure(){
        super("float",gnu.math.DFloNum.class);
    }
    
    public Object apply1(gnu.math.DFloNum arg){return arg;}
    
    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            if (arg instanceof gnu.math.DFloNum) {
                return apply1((gnu.math.DFloNum)arg);
            } else {
                throw new IllegalArgumentException("Non-float argument in float constructor");
            }
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to float constructor");
        }
    }

    public static FloatStructure getInstance(){
        if (instance == null) { instance=new FloatStructure(); }
        return instance;
    }
}

