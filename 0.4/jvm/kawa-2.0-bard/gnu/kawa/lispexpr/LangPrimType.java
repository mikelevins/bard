package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.math.IntNum;
import gnu.math.DFloNum;
import gnu.expr.*;
import gnu.kawa.reflect.LazyType;
import gnu.lists.Sequence;
import gnu.text.Char;
import gnu.kawa.reflect.InstanceOf;
import gnu.mapping.Procedure;
import gnu.mapping.Values;

/** Use to implement some special types that convert differently.
 * May not be needed/appropriate any more now that we support
 * arithmetic on a mix of Java and Kawa types.
 */

public class LangPrimType extends PrimType implements TypeValue {
    Language language;
    PrimType implementationType;

    public static final PrimType byteType = Type.byteType;
    public static final PrimType shortType = Type.shortType;
    public static final PrimType intType = Type.intType;
    public static final PrimType longType = Type.longType;
    public static final PrimType floatType = Type.floatType;
    public static final PrimType doubleType = Type.doubleType;
    public static final LangPrimType charType
        = new LangPrimType(Type.charType);
    public static final PrimType voidType = Type.voidType;

    static final ClassType scmCharType = ClassType.make("gnu.text.Char");

    public static final LangPrimType characterType
       = new LangPrimType(Type.intType);
    static { characterType.setName("character"); }
    public static final LangPrimType characterOrEofType
       = new LangPrimType(Type.intType);
    static { characterOrEofType.setName("character-or-eof"); }

    public static final LangPrimType stringCursorType
       = new LangPrimType(Type.intType);
    static { stringCursorType.setName("string-cursor"); }

    public LangPrimType(PrimType type) {
        super(type);
        implementationType = type;
    }

    public LangPrimType(PrimType type, Language language) {
        super(type);
        this.language = language;
        implementationType = type;
    }

    public LangPrimType(String nam, String sig, int siz, Class reflectClass) {
        super (nam, sig, siz, reflectClass);
    }

    public LangPrimType(String nam, String sig, int siz, Class reflectClass,
                        Language language) {
        this(nam, sig, siz, reflectClass);
        implementationType = Type.signatureToPrimitive(sig.charAt(0));
        this.language = language;
    }

    public Type getImplementationType() {
        return implementationType;
    }

    @Override
    public ClassType boxedType() {
        if (this == characterType)
            return scmCharType;
        if (this == characterOrEofType)
            return Type.objectType;
        return super.boxedType();
    }

    @Override
    public Object coerceFromObject(Object obj) {
        if (obj.getClass() == reflectClass)
            return obj;
        char sig1 = getSignature().charAt(0);
        switch (sig1) {
        case 'I':
            if (this == characterType || this == characterOrEofType) {
                int ival;
                if (obj instanceof Integer)
                    return obj;
                if (obj instanceof Char)
                    ival = ((Char) obj).intValue();
                else if (obj == Sequence.eofValue && this == characterOrEofType)
                    ival = -1;
                else
                    ival = ((Character) obj).charValue();
                return Integer.valueOf(ival);
            }
            break;
        case 'Z':
            return language.isTrue(obj) ? Boolean.TRUE : Boolean.FALSE;
        case 'C':
            return new Character(((Char) obj).charValue());
        case 'V':
            return Values.empty;
        }
        return super.coerceFromObject(obj);
    }

    public char charValue(Object value) {
        if (value instanceof Character)
            return ((Character) value).charValue();
        return  ((Char) value).charValue();
    }

    public void emitIsInstance(CodeAttr code) {
        char sig1 = getSignature().charAt(0);
        switch (sig1) {
        case 'I':
            String mname = this == characterType ? "isChar"
                : this == characterOrEofType ? "isCharOrEof" : null;
            if (mname != null) {
                code.emitInvokeStatic(scmCharType.getDeclaredMethod(mname,1));
                return;
            }
            break;
        case 'Z':
            code.emitPop(1);
            code.emitPushInt(1);
            return;
        case 'C':
            ClassType scmCharType = ClassType.make("gnu.text.Char");
            code.emitInvokeStatic(scmCharType.getDeclaredMethod("isChar",1));
            return;
        }
        super.emitIsInstance(code);
    }

    public void emitCoerceFromObject(CodeAttr code) {
        char sig1 = getSignature().charAt(0);
        switch (sig1) {
        case 'I':
            if (this == characterType || this == characterOrEofType) {
                Type top = code.topType();
                if (top == javalangCharacterType) {
                    code.emitInvokeVirtual(javalangCharacterType.getDeclaredMethod("charValue", 0));
                    return;
                }
                if (top == scmCharType) {
                    code.emitInvokeVirtual(scmCharType.getDeclaredMethod("intValue", 0));
                    return;
                }
            }
            String mname = this == characterType ? "castToCharacter"
                : this == characterOrEofType ? "castToCharacterOrEof" : null;
            if (mname != null) {
                code.emitInvokeStatic(scmCharType.getDeclaredMethod(mname,1));
                return;
            }
            break;
        case 'Z':
            Compilation.getCurrent().emitCoerceToBoolean();
            return;
        case 'C':
            if (code.topType() == javalangCharacterType) {
                code.emitInvokeVirtual(javalangCharacterType.getDeclaredMethod("charValue", 0));
                return;
            }
            Method charValueMethod = scmCharType.getDeclaredMethod("castToChar",1);
            code.emitInvokeStatic(charValueMethod);
            return;
        }
        super.emitCoerceFromObject(code);
    }

    public Object coerceToObject(Object obj) {
        char sig1 = getSignature().charAt(0);
        switch (sig1) {
        case 'I':
            if (this == characterType || this == characterOrEofType) {
                if (obj instanceof Integer) {
                    int ival = ((Integer) obj).intValue();
                    if (ival >= 0)
                        return Char.make(ival);
                    if (ival == -1 && this == characterOrEofType)
                        return Sequence.eofValue;
                }
                if (obj instanceof Char
                    && (obj == Sequence.eofValue && this == characterOrEofType))
                    return obj;
            }
            break;
        case 'Z':
            return language.booleanObject(((Boolean) obj).booleanValue());
        case 'C':
            if (obj instanceof Char)
                return obj;
            return Char.make(((Character) obj).charValue());
        case 'V':
            // Perhaps we should return Language.noValue() instead?
            return gnu.mapping.Values.empty;
        }
        return super.coerceToObject(obj);
    }

    @Override
    public Object convertToRaw(Object obj) {
        if (this == characterType || this == characterOrEofType) {
            if (obj instanceof Char)
                return Integer.valueOf(((Char) obj).intValue());
            if (obj == Sequence.eofValue && this == characterOrEofType)
                return Integer.valueOf(-1);
        }
        return obj;
    }

    public void emitCoerceToObject(CodeAttr code) {
        char sig1 = getSignature().charAt(0);
        Type argType = null;
        String cname = null;
        switch (sig1) {
        case 'I':
            String mname = this == characterType ? "make"
                : this == characterOrEofType ? "makeOrEof" : null;
            if (mname != null) {
                Method makeCharMethod = scmCharType.getDeclaredMethod(mname, 1);
                code.emitInvokeStatic(makeCharMethod);
            } else
                super.emitCoerceToObject(code);
            break;
        case 'Z':
            Compilation comp = Compilation.getCurrent();
            code.emitIfIntNotZero();
            comp.emitPushBoolean(true);
            code.emitElse();
            comp.emitPushBoolean(false);
            code.emitFi();
            break;
        case 'C':
            ClassType scmCharType = ClassType.make("gnu.text.Char");
            Method makeCharMethod = scmCharType.getDeclaredMethod("make", 1);
            code.emitInvokeStatic(makeCharMethod);
            break;
        default:
            super.emitCoerceToObject(code);
        }
        if (cname != null) {
            ClassType clas = ClassType.make(cname);
            Type[] args = { argType };
            code.emitInvokeStatic(clas.getDeclaredMethod("make", args));
        }
    }

    @Override
    public int compare(Type other) {
        if (other instanceof LazyType)
            other = ((LazyType) other).getValueType();
        char sig1 = getSignature().charAt(0);
        char sig2 = other.getSignature().charAt(0);
        if (sig1 == 'Z')
            return implementationType.compare(other);
        if (this == other)
            return 0;
        if (this == stringCursorType)
            return other == Type.objectType ? -1 : -3;
        if (this == charType) {
            if (other == characterType || other == characterOrEofType
                || other == scmCharType)
                return -1;
            return getImplementationType().compare(other);
        }
        if (this == characterType) {
            if (other == characterOrEofType)
                return -1;
            if (other == charType || sig2 == 'C')
                return 1;
            return scmCharType.compare(other);
        }
        if (this == characterOrEofType) {
            if (other == characterType
                || other == ClassType.make("gnu.lists.EofClass") // FIXME pre-alloate
                || other == charType || other == scmCharType || sig2 == 'C')
                return 1;
            return other == Type.objectType ? -1 : -3;
        }
        if (other instanceof PrimType) {
            if (sig1 == sig2)
                return 0;
            if (sig1 == 'V')
                return 1;
            if (sig2 == 'V')
                return -1;
        }
        if (sig1 == 'V')
            return 1;
        if (other instanceof LangObjType)
            return swappedCompareResult(other.compare(this));
        return super.compare(other);
    }

    @Override
    public int isCompatibleWithValue(Type valueType) {
        if (getSignature().charAt(0) == 'Z')
            return 1;
        if (this == charType && valueType == Type.charType)
            return 2;
        return super.isCompatibleWithValue(valueType);
    }

    public void emitTestIf(Variable incoming, Declaration decl,
                           Compilation comp) {
        CodeAttr code = comp.getCode();
        if (incoming != null)
            code.emitLoad(incoming);
        if (decl != null) {
            code.emitDup();
            decl.compileStore(comp);
        }
        emitIsInstance(code);
        code.emitIfIntNotZero();
    }

    public Expression convertValue(Expression value) {
        return null;
    }

    public void emitIsInstance(Variable incoming,
                               Compilation comp, Target target) {
        InstanceOf.emitIsInstance(this, incoming, comp, target);
    }

    public Procedure getConstructor() {
        return null;
    }

    public String encodeType(Language language) {
        if (this == characterType) return "character";
        if (this == characterOrEofType) return "character-or-eof";
        if (this == stringCursorType) return "string-cursor";
        return null;
    }
}
