package gnu.bytecode;

import java.util.List;

/** Used for object types that don't correspond to JVM types.
 * These are implemented by some implementation type (a ClassType),
 * but may have extra non-standard properties.
 */

public class SpecialObjectType extends ObjectType {
    protected ClassType implementationType;

    public SpecialObjectType(String name, ClassType implementationType) {
        super(name);
        this.implementationType = implementationType;
        this.setSignature(implementationType.getSignature());
    }

    @Override
    public Field getField(String name, int mask) {
        return implementationType.getField(name, mask);
    }

    @Override
    public Method getMethod(String name, Type[] arg_types) {
        return implementationType.getMethod(name, arg_types);
    }

    public Method getDeclaredMethod(String name, int argCount) {
        return implementationType.getDeclaredMethod(name, argCount);
    }

    @Override
    public int getMethods (Filter filter, int searchSupers,
                           List<Method> result) {
        return implementationType.getMethods(filter, searchSupers, result);
    }

    @Override
    public java.lang.Class getReflectClass() {
        return implementationType.getReflectClass();
    }

    @Override
    public Type getRealType() {
        return implementationType;
    }

    @Override
    public Type getImplementationType() {
        return implementationType;
    }

    @Override
    public int compare(Type other) {
        if (this == toStringType) {
            return other == Type.javalangStringType ? 0
                : other == Type.javalangObjectType ? -1 : 1;
        }
        return super.compare(other);
    }
}
