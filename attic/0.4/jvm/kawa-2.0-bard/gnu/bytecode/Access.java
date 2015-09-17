// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** Access flags. */
/* When using JDK 1.1, replace this class by java.lang.reflec.Modifiers. */

public class Access {
  static public final short PUBLIC      = 0x0001;
  static public final short PRIVATE     = 0x0002;
  static public final short PROTECTED   = 0x0004;
  static public final short STATIC      = 0x0008;
  static public final short FINAL       = 0x0010;
  static public final short SUPER       = 0x0020;
  static public final short SYNCHRONIZED= 0x0020;
  static public final short VOLATILE    = 0x0040;
  static public final short BRIDGE      = 0x0040;
  static public final short TRANSIENT   = 0x0080;
  static public final short VARARGS     = 0x0080;
  static public final short NATIVE      = 0x0100;
  static public final short INTERFACE   = 0x0200;
  static public final short ABSTRACT    = 0x0400;
  static public final short STRICT      = 0x0800;
  static public final short SYNTHETIC   = 0x1000;
  static public final short ANNOTATION  = 0x2000;
  static public final short ENUM        = 0x4000;
  // unassigned 0x8000

  /** Value for {@code toString}'s {@code kind} parameter when decoding
   * a class's access flags. */
  public static final char CLASS_CONTEXT = 'C';
  /** Value for {@code toString}'s {@code kind} parameter when decoding
   * access flags in {@code InnerClasses} attribute. */
  public static final char INNERCLASS_CONTEXT = 'I';
  /** Value for {@code toString}'s {@code kind} parameter when decoding
   * a field's access flags. */
  public static final char FIELD_CONTEXT = 'F';
  /** Value for {@code toString}'s {@code kind} parameter when decoding
   * a methods's access flags. */
  public static final char METHOD_CONTEXT = 'M';

  /** Mask of access flags valid for a class. */
  public static final short CLASS_MODIFIERS
    = (short)(PUBLIC|FINAL|SUPER|INTERFACE|ABSTRACT|SYNTHETIC|ANNOTATION|ENUM);
  /** Mask of access flags valid in an {@code InnerClasses} attribute. */
  public static final short INNERCLASS_MODIFIERS
    = (short)(PUBLIC|PRIVATE|PROTECTED|STATIC|FINAL|INTERFACE|ABSTRACT|SYNTHETIC|ANNOTATION|ENUM);
  /** Mask of access flags valid for a field. */
  public static final short FIELD_MODIFIERS
    = (short)(PUBLIC|PRIVATE|PROTECTED|STATIC|FINAL
	      |VOLATILE|TRANSIENT|SYNTHETIC|ENUM);
  /** Mask of access flags valid for a method. */
  public static final short METHOD_MODIFIERS
    = (short)(PUBLIC|PRIVATE|PROTECTED|STATIC|FINAL|SYNCHRONIZED
	      |BRIDGE|VARARGS|NATIVE|ABSTRACT|STRICT|SYNTHETIC);

  public static String toString(int flags)
  {
    return toString(flags, '\0');
  }

  /** Return a string naming the access bits in flags.
   * @param kind {@code CLASS_CONTEXT} for a class,
   *   {@code METHOD_CONTEXT} for a method,
   *   {@code FIELD_CONTEXT} for a field,
   *   {@code INNERCLASS_CONTEXT} in an {@code InnerClasses} attribute.
   */
  public static String toString(int flags, char kind)
  {
    short mask
      = (kind == CLASS_CONTEXT ? CLASS_MODIFIERS
	 : kind == INNERCLASS_CONTEXT ? INNERCLASS_MODIFIERS
	 : kind == FIELD_CONTEXT ? FIELD_MODIFIERS
	 : kind == METHOD_CONTEXT ? METHOD_MODIFIERS
	 : (CLASS_MODIFIERS|FIELD_MODIFIERS|METHOD_MODIFIERS));
    short bad_flags = (short) (flags & ~mask);
    flags &= mask;
    StringBuffer buf = new StringBuffer();
    if ((flags & PUBLIC) != 0)      buf.append(" public");
    if ((flags & PRIVATE) != 0)     buf.append(" private");
    if ((flags & PROTECTED) != 0)   buf.append(" protected");
    if ((flags & STATIC) != 0)      buf.append(" static");
    if ((flags & FINAL) != 0)       buf.append(" final");
    if ((flags & SYNCHRONIZED) != 0)
      buf.append(kind == 'C' ? " super" : " synchronized");
    if ((flags & VOLATILE) != 0)
      buf.append(kind == 'M' ? " bridge" : " volatile");
    if ((flags & TRANSIENT) != 0)
      buf.append(kind == 'M' ? " varargs" : " transient");
    if ((flags & NATIVE) != 0)      buf.append(" native");
    if ((flags & INTERFACE) != 0)   buf.append(" interface");
    if ((flags & ABSTRACT) != 0)    buf.append(" abstract");
    if ((flags & STRICT) != 0)      buf.append(" strict");
    if ((flags & ENUM) != 0)        buf.append(" enum");
    if ((flags & SYNTHETIC) != 0)   buf.append(" synthetic");
    if ((flags & ANNOTATION) != 0)  buf.append(" annotation");
    if (bad_flags != 0)
      {
	buf.append(" unknown-flags:0x");
	buf.append(Integer.toHexString(bad_flags));
      }
    return buf.toString();
  }
}
