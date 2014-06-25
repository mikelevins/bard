// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// SymbolStructure is a singleton that represents symbol values

public class SymbolStructure extends NativeStructure {

    private static SymbolStructure instance=null;

    SymbolStructure(){
        super("symbol",gnu.mapping.Symbol.class);
    }
    
    public Object apply1(gnu.mapping.Symbol arg){return arg;}
    public Object apply1(gnu.expr.Keyword arg){return arg.asSymbol();}
    public Object apply1(java.lang.String arg){return new gnu.mapping.SimpleSymbol(arg);}
    
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
                throw new IllegalArgumentException("Non-name argument in symbol constructor");
            }
        } else {
            throw new IllegalArgumentException("Wrong number of arguments to symbol constructor");
        }
    }


    public static SymbolStructure getInstance(){
        if (instance == null) { instance=new SymbolStructure(); }
        return instance;
    }
}

