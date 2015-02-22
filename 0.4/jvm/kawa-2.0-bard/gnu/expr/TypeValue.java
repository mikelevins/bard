// Copyright (c) 2001, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.Procedure;

/** A Type or a Type expression.
 * Can be used for higher-level types that do not map directly to a Type.
 */

public interface TypeValue extends java.lang.reflect.Type {
    /** The lower-level Type used to represent instances of this type. */
    public Type getImplementationType();

    /** Emit code for
     *  <tt>if (incoming instanceof this_type) decl = incoming ...</tt>.
     * This method is designed for <tt>typeswitch</tt> applications, where this
     * call is the first part of a conditional, so it must be followed
     * by calls to <tt>emitElse</tt> and <tt>emitFi</tt>.
     * @param incoming Contains the value we are testing to see if it has the
     *        type of <tt>this</tt>.  If null, use top-of-stack.
     *        May not be null if decl is non-null.
     * @param decl If non-null, assign value after coercion to <tt>Declaration</tt>.
     * @param comp The compilation state.
     */
    public void emitTestIf(Variable incoming, Declaration decl,
                           Compilation comp);

    /** Emit code for <tt>incoming instanceof this_type</tt>.
     * The implementation can use
     * {@link gnu.kawa.reflect.InstanceOf#emitIsInstance InstanceOf
     *   .emitIsInstance} which is a conveniece method that calls
     * {@link #emitTestIf emitTestIf}.
     * @param incoming Contains the value we are testing to see if it has the
     *        the type of 'this'.  If null, use top-of-stack.
     * @param comp The compilation state.
     * @param target Where to leave the result.
     */
    public void emitIsInstance(Variable incoming,
                               Compilation comp, Target target);

    /** Get the constructor function for this type.
     * Returns null if there is no contructor function.
     * Also returns null if this extends ClassType or ArrayType and
     * standard Java constructors (<init> methods) should be used.
     */
    public Procedure getConstructor ();

    /** Return converted expression or null. */
    public Expression convertValue (Expression value);

    /* #ifdef JAVA8 */
    default public String encodeType(Language language) { return null; }
    /* #else */
    // public String encodeType(Language language);
    /* #endif */
}
