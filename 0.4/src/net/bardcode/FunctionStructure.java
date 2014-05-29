// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import net.bardcode.Function;
import java.util.Arrays;
import java.util.List;

// FunctionStructure is a singleton that represents the structure of functions

public class FunctionStructure extends NativeStructure {

    private static FunctionStructure instance=null;

    FunctionStructure(){
        super("function",net.bardcode.Function.class);
    }
    
    @Override
    public Object applyN(Object[] args){
        Function result = null;
        java.lang.Boolean okay = true;
        for (int i = 0; i < args.length ; i++) {
            Object arg = args[i];
            if (!(arg instanceof net.bardcode.Type)) {
                okay = false;
                break;
            }
        }
        if (okay) {
            List sig = Arrays.asList(args);
            result = new Function(sig);
        } else {
            throw new IllegalArgumentException("Non-type argument to function constructor");
        }
        return result;
    }


    public static FunctionStructure getInstance(){
        if (instance == null) { instance=new FunctionStructure(); }
        return instance;
    }
}

