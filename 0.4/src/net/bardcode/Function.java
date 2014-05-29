package net.bardcode;
import java.util.List;
import gnu.mapping.*;
import org.pcollections.HashPMap;
import org.pcollections.HashTreePMap;
import net.bardcode.Type;

public class Function {
    List<Type> signature;
    HashPMap<List<Type>,MethodProc> methods;

    public Function(List<Type> sig){
        signature = sig;
        methods = HashTreePMap.empty();
    }

    public List<Type> getSignature(){
        return signature;
    }

    public Function addMethod(List<Type>sig ,MethodProc method){
        methods = methods.plus(sig,method);
        return this;
    }
}


