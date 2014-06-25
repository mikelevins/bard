package net.bardcode;
import java.util.ArrayList;
import gnu.mapping.Symbol;
import org.pcollections.HashTreePMap;
import org.pcollections.HashPMap;

public class BardClass implements Type {

    public static HashPMap<Symbol,BardClass> classes = HashTreePMap.empty();

    gnu.mapping.Symbol name;
    ArrayList superclasses;
    ArrayList members;

    public BardClass(Symbol nm){
        name = nm;
        classes = classes.plus(nm,this);
        superclasses = new ArrayList();
        members = new ArrayList();
    }

    public static BardClass findClass(Symbol nm){
        return classes.get(nm);
    }
    
    public Symbol getName() { return name; }

}
