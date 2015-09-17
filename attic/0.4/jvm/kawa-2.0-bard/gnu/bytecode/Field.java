// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class Field extends Location
    implements AttrContainer, Member
               // FUTURE: javax.lang.model.VariableElement
{
    int flags;
    Field next;

    Attribute attributes;
    public final Attribute getAttributes () { return attributes; }
    public final void setAttributes (Attribute attributes)
    { this.attributes = attributes; }

    /** The class that contains this field. */
    ClassType owner;

    /** If non-null, the interned source-file (unmangled) name of the field. */
    String sourceName;

    /** If non-null, a cached version of the Field for reflection. */
    java.lang.reflect.Field rfield;

    /** Add a new Field to a ClassType. */
    public Field(ClassType ctype) {
        if (ctype.last_field == null)
            ctype.fields = this;
        else
            ctype.last_field.next = this;
        ctype.last_field = this;
        ctype.fields_count++;
        owner = ctype;
    }

    public final ClassType getDeclaringClass() {
        return owner;
    }

    public final ConstantPool getConstants() {
        return owner.constants;
    }
    
    public final void setStaticFlag(boolean is_static) {
        if (is_static)
            flags |= Access.STATIC;
        else
            flags ^= ~Access.STATIC;
    }

    public final boolean getStaticFlag() {
        return (flags & Access.STATIC) != 0;
    }

    public final int getFlags() {
        return flags;
    }
  
    public final int getModifiers() {
        return flags;
    }

    public final void setModifiers(int modifiers) {
        flags = modifiers;
    }
  
    @Override
    public Type getType() {
        synchronized (this) {
            Type t = super.getType();
            if (t == null && rfield != null) {
                t = Type.make(rfield.getType(), rfield.getGenericType());
                super.setType(t);
            }
            return t;
        }
    }

    void write(DataOutputStream dstr, ClassType classfile)
        throws java.io.IOException {
        dstr.writeShort (flags);
        dstr.writeShort (name_index);
        dstr.writeShort (signature_index);

        Attribute.writeAll(this, dstr);
    }
  
    void assign_constants(ClassType classfile) {
        ConstantPool constants = classfile.constants;
        Type t = getType();
        String signature = t.getSignature();
        String genericSignature = t.getGenericSignature();
        if (genericSignature != null && ! genericSignature.equals(signature)) {
            SignatureAttr attr = new SignatureAttr(genericSignature);
            attr.addToFrontOf(this);
        }
        if (name_index == 0 && name != null)
            name_index = constants.addUtf8(name).index;
        if (signature_index == 0 && t != null)
            signature_index = constants.addUtf8(signature).index;
        Attribute.assignConstants(this, classfile);
    }

    public synchronized java.lang.reflect.Field getReflectField()
        throws java.lang.NoSuchFieldException {
        if (rfield == null)
            rfield = owner.getReflectClass().getDeclaredField(getName());
        return rfield;
    }

    public <T extends java.lang.annotation.Annotation>
    T getAnnotation(Class<T> clas) {
        T ann = RuntimeAnnotationsAttr.getAnnotation(this, clas);
        if (ann != null)
            return ann;
        return rfield == null ? null : rfield.getAnnotation(clas);
    }

    public void setSourceName(String name) {
        sourceName = name;
    }

    public String getSourceName() {
        if (sourceName == null)
            sourceName = getName().intern();
        return sourceName;
    }

    /** Find a field with the given name.
     * @param fields list of fields to search
     * @param name (interned source) name of field to look for
     */
    public static Field searchField(Field fields, String name) {
        for (; fields != null;  fields = fields.next) {
            if (fields.getSourceName() == name)
                return fields;
        }
        return null;
    }

    public final Field getNext() {
        return next;
    }

    /** Set the ConstantValue attribute for this field.
     * @param value the value to use for the ConstantValue attribute
     *   of this field
     * @param ctype the class that contains this field
     * This field's type is used to determine the kind of constant.
     */
    public final void setConstantValue(Object value, ClassType ctype) {
        ConstantPool cpool = ctype.constants;
        if (cpool == null)
            ctype.constants = cpool = new ConstantPool();
        char sig1 = getType().getSignature().charAt(0);
        CpoolEntry entry;
        switch (sig1) {
        case 'Z':
            entry = cpool.addInt(PrimType.booleanValue(value)? 1 : 0);
            break;
        case 'C':
            if (value instanceof Character) {
                entry = cpool.addInt(((Character) value).charValue());
                break;
            }
            /// else fall through ...
        case 'B':
        case 'S':
        case 'I':
            entry = cpool.addInt(((Number) value).intValue());
            break;
        case 'J':
            entry = cpool.addLong(((Number) value).longValue());
            break;
        case 'F':
            entry = cpool.addFloat(((Number) value).floatValue());
            break;
        case 'D':
            entry = cpool.addDouble(((Number) value).doubleValue());
            break;
        default:
            entry = cpool.addString(value.toString());  break;
        }
        ConstantValueAttr attr = new ConstantValueAttr(entry.getIndex());
        attr.addToFrontOf(this);
    }

    public boolean hasConstantValueAttr() {
        return Attribute.get(this, "ConstantValue") != null;
    }

    public String toString() {
        StringBuffer sbuf = new StringBuffer(100);
        sbuf.append("Field:");
        sbuf.append(getDeclaringClass().getName());
        sbuf.append('.');
        sbuf.append(name);
        return sbuf.toString();
    }
}
