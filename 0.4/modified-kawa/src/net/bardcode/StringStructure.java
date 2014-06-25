// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// StringStructure is a singleton that represents symbol values

public class StringStructure extends NativeStructure {

    private static StringStructure instance=null;

    StringStructure(){
        super("string", java.lang.String.class);
    }
    
    String constructString (Object[] args){
        if (args.length==0){
            return "";
        } else if (args.length==1) {
            Object arg = args[0];
            if (arg instanceof java.lang.String) {
                return (java.lang.String)arg;
            } else if (arg instanceof gnu.expr.Keyword) {
                gnu.expr.Keyword kw = (gnu.expr.Keyword)arg;
                return kw.getName();
            } else if (arg instanceof gnu.mapping.Symbol) {
                gnu.mapping.Symbol sym = (gnu.mapping.Symbol)arg;
                return sym.getName();
            } else if (arg instanceof gnu.text.Char) {
                gnu.text.Char ch = (gnu.text.Char)arg;
                int[] chs = { ch.intValue() };
                return new java.lang.String(chs,0,1);
            } else {
                throw new IllegalArgumentException("Wrong argument structure in string constructor");
            }
        } else {
            throw new IllegalArgumentException("Wrong argument structures in string constructor");
        }
    }

    @Override
    public Object applyN(Object[] args){
        return constructString (args);
    }


    public static StringStructure getInstance(){
        if (instance == null) { instance=new StringStructure(); }
        return instance;
    }
}

