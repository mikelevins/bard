// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// CharacterStructure is a singleton that represents character values

public class CharacterStructure extends NativeStructure {

    private static CharacterStructure instance=null;

    CharacterStructure(){
        super("character",gnu.text.Char.class);
    }
    
    public Object apply1(gnu.math.IntNum arg){return gnu.text.Char.make(arg.intValue());}
    public Object apply1(gnu.text.Char arg){return arg;}
    public Object apply1(java.lang.String arg){
        if (arg.length()==1) {
            char ch = arg.charAt(0);
            return gnu.text.Char.make(ch);
        } else {
            throw new IllegalArgumentException("Too many character arguments in character constructor");
        }
    }
    
    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            if(arg instanceof gnu.text.Char) {
                return apply1((gnu.text.Char)arg);
            } else if (arg instanceof gnu.math.IntNum) {
                return apply1((gnu.math.IntNum)arg);
            } else if(arg instanceof java.lang.String) {
                return apply1((java.lang.String)arg);
            } else {
                throw new IllegalArgumentException("Can't convert to character");
            }
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to character constructor");
        }
    }


    public static CharacterStructure getInstance(){
        if (instance == null) { instance=new CharacterStructure(); }
        return instance;
    }
}

