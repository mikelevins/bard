// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// MapStructure is a singleton that represents the structure of maps

public class MapStructure extends NativeStructure {

    private static MapStructure instance=null;

    MapStructure(){
        super("map",org.pcollections.HashPMap.class);
    }
    
    @Override
    public Object applyN(Object[] args){
        org.pcollections.HashPMap result = org.pcollections.HashTreePMap.empty();
        int count = args.length;
        // an odd number of args is incorrect
        if ( (count % 2) == 0 ){
            for (int i = 0; i < count ; i+=2){
                Object key = args[i];
                Object val = args[i+1];
                result = result.plus(key,val);
            }
        } else {
            throw new IllegalArgumentException("Odd number of arguments to map constructor");
        }
        return result;
    }


    public static MapStructure getInstance(){
        if (instance == null) { instance=new MapStructure(); }
        return instance;
    }
}

