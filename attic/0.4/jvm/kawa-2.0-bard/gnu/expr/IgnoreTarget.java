package gnu.expr;

import gnu.bytecode.Type;
import gnu.bytecode.CodeAttr;

public class IgnoreTarget extends Target {
    public Type getType() { return Type.voidType; }

    public void compileFromStack(Compilation comp, Type stackType) {
        if (! stackType.isVoid()) {
            CodeAttr code = comp.getCode();
            if (code.reachableHere())
                code.emitPop(1);
        }
    }
}
