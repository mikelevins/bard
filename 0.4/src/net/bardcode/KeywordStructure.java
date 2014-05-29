// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// KeywordStructure is a singleton that represents keyword values

public class KeywordStructure extends NativeStructure {

    private static KeywordStructure instance=null;

    KeywordStructure(){
        super("keyword",gnu.expr.Keyword.class);
    }
    
    public Object apply1(gnu.mapping.Symbol arg){return gnu.expr.Keyword.make(arg.getName());}
    public Object apply1(gnu.expr.Keyword arg){return arg;}
    public Object apply1(java.lang.String arg){return gnu.expr.Keyword.make(arg);}
    
    @Override
    public Object applyN(Object[] args){
        if (args.length==1) {
            Object arg = args[0];
            if(arg instanceof gnu.expr.Keyword) {
                return apply1((gnu.expr.Keyword)arg);
            } else if (arg instanceof gnu.mapping.Symbol) {
                return apply1((gnu.mapping.Symbol)arg);
            } else if(arg instanceof java.lang.String) {
                return apply1((java.lang.String)arg);
            } else {
                throw new IllegalArgumentException("Non-name argument in keyword constructor");
            }
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to keyword constructor");
        }
    }


    public static KeywordStructure getInstance(){
        if (instance == null) { instance=new KeywordStructure(); }
        return instance;
    }
}

