// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// BoxStructure is a singleton that represents the structure of boxes

public class BoxStructure extends NativeStructure {

    private static BoxStructure instance=null;

    BoxStructure(){
        super("box",net.bardcode.Box.class);
    }
    
    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            return new Box(arg);
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to box constructor");
        }
    }


    public static BoxStructure getInstance(){
        if (instance == null) { instance=new BoxStructure(); }
        return instance;
    }
}

