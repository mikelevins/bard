// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;
import net.bardcode.Type;

// instances of Structure are Bard objects that represent
// structures.

public abstract class Structure extends ProcedureN implements Type {
    String name;
    public String getName(){return name;};

    public static final gnu.lists.EmptyList nothing = gnu.lists.LList.Empty;
    public static final NoneStructure none = NoneStructure.getInstance();
    public static final UndefinedStructure undefined = UndefinedStructure.getInstance();
    public static final BooleanStructure booleanStructure = BooleanStructure.getInstance();
    public static final CharacterStructure characterStructure = CharacterStructure.getInstance();
    public static final FloatStructure floatStructure = FloatStructure.getInstance();
    public static final IntegerStructure integerStructure = IntegerStructure.getInstance();
    public static final KeywordStructure keywordStructure = KeywordStructure.getInstance();
    public static final SymbolStructure symbolStructure = SymbolStructure.getInstance();
    public static final StringStructure stringStructure = StringStructure.getInstance();
    public static final ConsStructure consStructure = ConsStructure.getInstance();
    public static final VectorStructure vectorStructure = VectorStructure.getInstance();
    public static final URIStructure uriStructure = URIStructure.getInstance();
    public static final MethodStructure methodStructure = MethodStructure.getInstance();
    public static final BoxStructure boxStructure = BoxStructure.getInstance();
    public static final ClassStructure classStructure = ClassStructure.getInstance();
    public static final SeqStructure seqStructure = SeqStructure.getInstance();
    public static final MapStructure mapStructure = MapStructure.getInstance();
    public static final FunctionStructure functionStructure = FunctionStructure.getInstance();
    public static final ProtocolStructure protocolStructure = ProtocolStructure.getInstance();
    
    public static Structure structureOf(Object obj){
        if (obj == gnu.lists.LList.Empty) {
            return net.bardcode.Structure.none;
        } else if(obj == none) {
            return net.bardcode.Structure.none;
        } else if(obj instanceof net.bardcode.UndefinedStructure) {
            return net.bardcode.Structure.undefined;
        } else if (obj instanceof java.lang.Boolean) {
            return net.bardcode.Structure.booleanStructure;
        } else if (obj instanceof gnu.text.Char) {
            return net.bardcode.Structure.characterStructure;
        } else if (obj instanceof gnu.math.DFloNum) {
            return net.bardcode.Structure.floatStructure;
        } else if (obj instanceof gnu.math.IntNum) {
            return net.bardcode.Structure.integerStructure;
        } else if (obj instanceof gnu.math.IntNum) {
            return net.bardcode.Structure.integerStructure;
        } else if (obj instanceof gnu.expr.Keyword) {
            return net.bardcode.Structure.keywordStructure;
            // Keyword is a subclass of Symbol, but as long as we test for it first,
            // the symbol test will correctly identify symbols that are not keywords
        } else if (obj instanceof gnu.mapping.Symbol) {
            return net.bardcode.Structure.symbolStructure;
        } else if (obj instanceof java.lang.String) {
            return net.bardcode.Structure.stringStructure;
        } else if (obj instanceof gnu.lists.LList) {
            return net.bardcode.Structure.consStructure;
        } else if (obj instanceof gnu.lists.FVector) {
            return net.bardcode.Structure.vectorStructure;
        } else if (obj instanceof gnu.text.URIPath) {
            return net.bardcode.Structure.uriStructure;
        } else if (obj instanceof gnu.mapping.MethodProc) {
            return net.bardcode.Structure.methodStructure;
        } else if (obj instanceof net.bardcode.Box) {
            return net.bardcode.Structure.boxStructure;
        } else if (obj instanceof net.bardcode.BardClass) {
            return net.bardcode.Structure.classStructure;
        } else if (obj instanceof org.pcollections.HashPMap) {
            return net.bardcode.Structure.mapStructure;
        } else if (obj instanceof org.pcollections.TreePVector) {
            return net.bardcode.Structure.seqStructure;
        } else if (obj instanceof net.bardcode.Function) {
            return net.bardcode.Structure.functionStructure;
        } else if (obj instanceof net.bardcode.Protocol) {
            return net.bardcode.Structure.protocolStructure;
        } else {
            return undefined;
        }
    };
    
    public static Boolean isUndefined(Object obj){
        return (obj instanceof UndefinedStructure);
    };
    
    public static Boolean isNothing(Object obj){
        return (obj == gnu.lists.LList.Empty);
    };
    
    public static Boolean isBoolean(Object obj){
        return (obj instanceof Boolean);
    };
    
    public static Boolean isCharacter(Object obj){
        return (obj instanceof gnu.text.Char);
    };
    
    public static Boolean isFloat(Object obj){
        return (obj instanceof gnu.math.DFloNum);
    };
    
    public static Boolean isInteger(Object obj){
        return (obj instanceof gnu.math.IntNum);
    };
    
    public static Boolean isKeyword(Object obj){
        return (obj instanceof gnu.expr.Keyword);
    };
    
    public static Boolean isSymbol(Object obj){
        return ((obj instanceof gnu.mapping.Symbol)&&(!(obj instanceof gnu.expr.Keyword)));
    };
    
    public static Boolean isString(Object obj){
        return (obj instanceof java.lang.String);
    };
    
    public static Boolean isCons(Object obj){
        return (obj instanceof gnu.lists.LList);
    };
    
    public static Boolean isVector(Object obj){
        return (obj.getClass() == gnu.lists.FVector.class);
    };
    
    public static Boolean isURI(Object obj){
        return (obj instanceof gnu.text.URIPath);
    };
    
    public static Boolean isMethod(Object obj){
        return (obj instanceof gnu.mapping.MethodProc);
    };
    
    public static Boolean isBox(Object obj){
        return (obj instanceof net.bardcode.Box);
    };
    
    public static Boolean isClass(Object obj){
        return (obj instanceof net.bardcode.BardClass);
    };
    
    public static Boolean isMap(Object obj){
        return (obj instanceof org.pcollections.HashPMap);
    };
    
    public static Boolean isSeq(Object obj){
        return (obj instanceof org.pcollections.TreePVector);
    };
    
    public static Boolean isFunction(Object obj){
        return (obj instanceof net.bardcode.Function);
    };
    
    public static Boolean isProtocol(Object obj){
        return (obj instanceof net.bardcode.Protocol);
    };
    
}

