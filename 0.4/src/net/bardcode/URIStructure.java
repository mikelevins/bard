// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// URIStructure is a singleton that represents uri values

public class URIStructure extends NativeStructure {

    private static URIStructure instance=null;

    URIStructure(){
        super("uri",gnu.text.URIPath.class);
    }
    
    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            if(arg instanceof gnu.text.URIPath) {
                return (gnu.text.URIPath)arg;
            } else {
                return gnu.text.URIPath.makeURI(arg);
            }
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to uri constructor");
        }
    }


    public static URIStructure getInstance(){
        if (instance == null) { instance=new URIStructure(); }
        return instance;
    }
}

