// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// ConsStructure is a singleton that represents cons values

public class ConsStructure extends NativeStructure {

    private static ConsStructure instance=null;

    ConsStructure(){
        super("cons", gnu.lists.LList.class);
    }
    
    @Override
    public Object applyN(Object[] args){
        if (args.length == 2){
            return new gnu.lists.Pair(args[0], args[1]);
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to cons");
        }
    }


    public static ConsStructure getInstance(){
        if (instance == null) { instance=new ConsStructure(); }
        return instance;
    }
}

